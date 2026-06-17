public class Spaceship {
    // Moving data
    public double Altitude { get; set; } 
    public double Velocity { get; set; } 
    public double EngineThrust { get; set; } 

    // State Flags
    public bool IsOrbiting { get; set; }
    public bool HasClearance { get; set; } // Added: Flag to check if clearance is granted
    public bool IsDescending { get; set; }
    public bool IsLanded { get; set; }
    public bool HasCrashed { get; set; }

    // Contextual Data
    public string LandingZoneCoordinates { get; set; } // Nullable
    public string ClearanceCode { get; set; }          // Added: Nullable clearance token/code
    public string CrashReason { get; set; }            // Nullable
}