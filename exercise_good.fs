// ── 1. Units of Measure ───────────────────────────────────────────────────────
// The compiler tracks units through every arithmetic expression.
// Adding meters to feet, or meters to seconds, is a compile error — not a runtime bug.
// (NASA lost the Mars Climate Orbiter in 1999 because of a feet/meters mismatch.)
[<Measure>] type m   // metres
[<Measure>] type ft  // feet
[<Measure>] type s   // seconds


// ── 2. Make the clearance token a type, not a plain string ────────────────────
// A function that requires a ClearanceToken cannot be called with just any string.
// You must go through the issuing authority — the type IS the proof of authorisation.
type ClearanceToken = ClearanceToken of Code: string


// ── 3. State-specific data ────────────────────────────────────────────────────
// Each record only carries the fields that make sense for that state.
// There is no "TrackingNumber" on an OrbitData, no "TargetLandingZone" on a DescentData.
// Compare this to a single class with lots of nullable / sometimes-relevant fields.
type OrbitData = { TargetLandingZone: string }

type DescentData = {
    Altitude:     float<m>
    Velocity:     float<m/s>
    EngineThrust: float          // 0.0 = off, 1.0 = full
}


// ── 4. The state machine as a discriminated union ─────────────────────────────
// Exactly one of these cases is true at any moment — the compiler enforces it.
// You cannot be both Landed and Descending. You cannot forget to handle a state
// when you add a new one: every match site will warn you immediately.
type Spaceship =
    | Orbiting   of OrbitData
    | Descending of DescentData
    | Landed     of FinalZone: string
    | Crashed    of CrashReason: string


// ── 5. Commands ───────────────────────────────────────────────────────────────
// All actions mission control can issue live in one type.
// Adding a new command means adding a case here; the compiler will point at every
// match that needs updating — no runtime surprises from unhandled messages.
type Command =
    | BeginDescent   of ClearanceToken
    | ApplyThrust    of Delta: float
    | CutEngines
    | EmergencyAbort of Reason: string


// ── 6. The transition function ────────────────────────────────────────────────
// Pure function: (current state, command) → next state.
// No mutation. No side effects. No shared mutable variables.
// Easy to test: call it with any (state, command) pair and inspect the result.
// Easy to replay: store the command list and re-run it to reconstruct any past state.
let transition (ship: Spaceship) (cmd: Command) : Spaceship =
    match ship, cmd with

    // Orbiting + valid clearance → begin descent
    // The 'when' guard rejects empty tokens at the boundary;
    // a smarter design would make an empty ClearanceToken unrepresentable at construction time.
    | Orbiting _, BeginDescent (ClearanceToken code) when code <> "" ->
        Descending { Altitude = 10_000.0<m>; Velocity = 200.0<m/s>; EngineThrust = 0.5 }

    // Descending + thrust adjustment → recompute physics, then decide outcome
    | Descending data, ApplyThrust delta ->
        let thrust   = System.Math.Clamp(data.EngineThrust + delta, 0.0, 1.0)
        let velocity = data.Velocity - (thrust * 50.0<m/s>)   // more thrust = slower descent
        let altitude = data.Altitude - velocity
        if   altitude <= 0.0<m> && velocity < 10.0<m/s> then  // soft landing
            Landed (sprintf "Touched down at %.0f m/s" velocity)
        elif altitude <= 0.0<m> then                           // too fast
            Crashed (sprintf "Impact at %.1f m/s — hull failure" velocity)
        else
            // 'with' creates a new record with only the listed fields changed;
            // all other fields are copied from 'data' — immutable update
            Descending { data with Altitude = altitude; Velocity = velocity; EngineThrust = thrust }

    // Cutting engines mid-descent is always fatal
    | Descending _, CutEngines ->
        Crashed "Engines cut — free fall"

    // Emergency abort is valid from any state (note the wildcard '_' on the left)
    | _, EmergencyAbort reason ->
        Crashed reason

    // All other (state, command) combinations are illegal transitions.
    // Returning 'ship' unchanged means they are silently ignored —
    // an alternative design would return a Result to surface the error.
    | _ -> ship


// ── 7. Running a mission ──────────────────────────────────────────────────────
// List.fold threads 'transition' through a sequence of commands,
// starting from the initial state and producing the final state.
// The intermediate states can be recovered by using List.scan instead.
let run (commands: Command list) : Spaceship =
    let initial = Orbiting { TargetLandingZone = "Pad 39A" }
    List.fold transition initial commands

// Example mission (send to FSI with Alt+Enter):
// run [
//     BeginDescent (ClearanceToken "GO")
//     ApplyThrust 0.2
//     ApplyThrust 0.4
//     ApplyThrust 0.3
// ]
