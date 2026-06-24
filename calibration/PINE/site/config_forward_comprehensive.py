"""
Configuration for COMPREHENSIVE FORWARD LSTM model
Predicts ALL energy and water cycle variables for full conservation checking

COMPREHENSIVE FORWARD Problem:
- Input: Parameters (NoahMP params) + Forcing variables (time series)
- Output: Energy/water cycle variables for conservation validation
- Goal: Validate model follows physical conservation laws

The LSTM/emulator side operates exclusively at DAILY resolution.
(Noah-MP itself still runs at 30-min cadence; only the emulator I/O is daily.)
"""

# =============================================================================
# TEMPORAL RESOLUTION CONFIGURATION
# =============================================================================

# Fixed: the emulator uses daily aggregates.
TEMPORAL_RESOLUTION = 'daily'


def get_timestep_seconds():
    """Emulator timestep, seconds. Daily-only."""
    return 86400

# =============================================================================
# TIME CONFIGURATION
# =============================================================================

# Full data time range
TIME_RANGE_FULL = {
    'start': '2005-01-01',
    'end': '2009-12-31'
}

# Calibration time range (first year)
TIME_RANGE_CALIBRATION = {
    'start': '2005-01-01',
    'end': '2006-12-31'
}

# Validation time range (second year) 
TIME_RANGE_VALIDATION = {
    'start': '2007-01-01',
    'end': '2009-12-30'
}

# =============================================================================
# COMPREHENSIVE TARGET VARIABLES CONFIGURATION
# =============================================================================

# Core Energy Balance Variables (for energy conservation check)
ENERGY_BALANCE_TARGETS = [
    {
        'name': 'FSA',
        'aggregation': 'mean',
        'description': 'Total absorbed SW radiation (W/m2)',
        'category': 'energy_balance'
    },
    {
        'name': 'FIRA',
        'aggregation': 'mean',
        'description': 'Total net LW radiation to atmosphere (W/m2)',
        'category': 'energy_balance'
    },
    {
        'name': 'HFX',
        'aggregation': 'mean',
        'description': 'Total sensible heat to atmosphere (W/m2)',
        'category': 'energy_balance'
    },
    {
        'name': 'LH',
        'aggregation': 'mean',
        'description': 'Total latent heat to atmosphere (W/m2)',
        'category': 'energy_balance'
    },
    {
        'name': 'GRDFLX',
        'aggregation': 'mean',
        'description': 'Heat flux into the soil (W/m2)',
        'category': 'energy_balance'
    },
]

# Energy Component Variables (for detailed energy budget)
ENERGY_COMPONENT_TARGETS = [
    {
        'name': 'SAV',
        'aggregation': 'mean',
        'description': 'Solar radiation absorbed by canopy (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'SAG',
        'aggregation': 'mean',
        'description': 'Solar radiation absorbed by ground (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'IRC',
        'aggregation': 'mean',
        'description': 'Canopy net LW rad (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'SHC',
        'aggregation': 'mean',
        'description': 'Canopy sensible heat (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'EVC',
        'aggregation': 'mean',
        'description': 'Canopy evap heat (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'IRG',
        'aggregation': 'mean',
        'description': 'Below-canopy ground net LW rad (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'SHG',
        'aggregation': 'mean',
        'description': 'Below-canopy ground sensible heat (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'EVG',
        'aggregation': 'mean',
        'description': 'Below-canopy ground evap heat (W/m2)',
        'category': 'energy_components'
    },
    {
        'name': 'GHV',
        'aggregation': 'mean',
        'description': 'Below-canopy ground heat to soil (W/m2)',
        'category': 'energy_components'
    },
]

# Water Flux Variables (for water balance)
WATER_FLUX_TARGETS = [
    {
        'name': 'ECAN',
        'aggregation': 'mean',
        'description': 'Canopy water evaporation rate (mm/s)',
        'category': 'water_fluxes'
    },
    {
        'name': 'ETRAN',
        'aggregation': 'mean',
        'description': 'Transpiration rate (mm/s)',
        'category': 'water_fluxes'
    },
    {
        'name': 'EDIR',
        'aggregation': 'mean',
        'description': 'Direct from soil evaporation rate (mm/s)',
        'category': 'water_fluxes'
    },
    {
        'name': 'UGDRNOFF',
        'output_name': 'UGDRNOFF_RATE',
        'aggregation': 'last',
        'description': 'Underground runoff rate (mm/day)',
        'category': 'water_fluxes',
        'convert_accumulated_to_rate': True
    },
    {
        'name': 'SFCRNOFF',
        'output_name': 'SFCRNOFF_RATE',
        'aggregation': 'last',
        'description': 'Surface runoff rate (mm/day)',
        'category': 'water_fluxes',
        'convert_accumulated_to_rate': True
    },
]

# Water Storage Variables
WATER_STORAGE_TARGETS = [
    {
        'name': 'SOIL_M',
        'aggregation': 'mean',
        'layer': 0,
        'description': 'Volumetric soil moisture Layer 1 (m3/m3)',
        'category': 'water_storage'
    },
    {
        'name': 'SOIL_M',
        'aggregation': 'mean',
        'layer': 1,
        'description': 'Volumetric soil moisture Layer 2 (m3/m3)',
        'category': 'water_storage',
        'output_name': 'SOIL_M_L2'
    },
    {
        'name': 'SOIL_M',
        'aggregation': 'mean',
        'layer': 2,
        'description': 'Volumetric soil moisture Layer 3 (m3/m3)',
        'category': 'water_storage',
        'output_name': 'SOIL_M_L3'
    },
    {
        'name': 'SOIL_M',
        'aggregation': 'mean',
        'layer': 3,
        'description': 'Volumetric soil moisture Layer 4 (m3/m3)',
        'category': 'water_storage',
        'output_name': 'SOIL_M_L4'
    },
    {
        'name': 'CANLIQ',
        'aggregation': 'mean',
        'description': 'Canopy liquid water content (mm)',
        'category': 'water_storage'
    },
]

# Temperature Variables (for energy/water coupling)
TEMPERATURE_TARGETS = [
    {
        'name': 'SOIL_T',
        'aggregation': 'mean',
        'layer': 0,
        'description': 'Soil temperature Layer 1 (K)',
        'category': 'temperature'
    },
    {
        'name': 'SOIL_T',
        'aggregation': 'mean',
        'layer': 1,
        'description': 'Soil temperature Layer 2 (K)',
        'category': 'temperature',
        'output_name': 'SOIL_T_L2'
    },
    {
        'name': 'TG',
        'aggregation': 'mean',
        'description': 'Ground temperature (K)',
        'category': 'temperature'
    },
    {
        'name': 'TV',
        'aggregation': 'mean',
        'description': 'Vegetation temperature (K)',
        'category': 'temperature'
    },
    {
        'name': 'TRAD',
        'aggregation': 'mean',
        'description': 'Surface radiative temperature (K)',
        'category': 'temperature'
    },
]

# Combine all target variables
TARGET_VARIABLES = (
    ENERGY_BALANCE_TARGETS +
    ENERGY_COMPONENT_TARGETS +
    WATER_FLUX_TARGETS +
    WATER_STORAGE_TARGETS +
    TEMPERATURE_TARGETS
)

# =============================================================================
# MAIN CALIBRATION TARGETS
# =============================================================================
# Subset of TARGET_VARIABLES that the calibration loss compares against
# observations. Plotting / metrics / scatter focus on these. Every name listed
# here MUST also be in TARGET_VARIABLES (or be a single-layer SOIL_M alias) and
# MUST have a matching column in the observation CSV.
#
# To calibrate against different variables (e.g. add GPP), change this list and
# re-run preprocessing -> training -> calibration. See docs/customization.md.
MAIN_TARGETS = ['SOIL_M', 'LH', 'HFX']

# =============================================================================
# FORCING VARIABLES CONFIGURATION (All 8 variables from LDASIN files)
# =============================================================================

# All 8 forcing variables from original LDASIN files
FORCING_VARIABLES = [
    {
        'name': 'T2D',
        'source_name': 'T2D',
        'noahmp_name': 'T2MV',
        'aggregation': 'mean',
        'description': '2m air temperature (K)'
    },
    {
        'name': 'Q2D',
        'source_name': 'Q2D',
        'noahmp_name': 'Q2D',
        'aggregation': 'mean',
        'description': '2m specific humidity (kg/kg)'
    },
    {
        'name': 'PSFC',
        'source_name': 'PSFC',
        'noahmp_name': 'PSFC',
        'aggregation': 'mean',
        'description': 'Surface pressure (Pa)'
    },
    {
        'name': 'U2D',
        'source_name': 'U2D',
        'noahmp_name': 'U2D',
        'aggregation': 'mean',
        'description': '2m U-wind component (m/s)'
    },
    {
        'name': 'V2D',
        'source_name': 'V2D',
        'noahmp_name': 'V2D',
        'aggregation': 'mean',
        'description': '2m V-wind component (m/s)'
    },
    {
        'name': 'LWDOWN',
        'source_name': 'LWDOWN',
        'noahmp_name': 'LWFORC',
        'aggregation': 'mean',
        'description': 'Downward longwave radiation (W/m2)'
    },
    {
        'name': 'SWDOWN',
        'source_name': 'SWDOWN',
        'noahmp_name': 'SWFORC',
        'aggregation': 'mean',
        'description': 'Downward shortwave radiation (W/m2)'
    },
    {
        'name': 'RAINRATE',
        'source_name': 'RAINRATE',
        'noahmp_name': 'RAINRATE',
        'aggregation': 'sum',
        'description': 'Precipitation rate (mm/s raw, mm/day when daily)'
    },
]

# Add temporal features
ADD_TIME_FEATURES = True  # Add day of year (normalized)
ADD_MONTH_FEATURE = False  # Add month as one-hot encoding

# =============================================================================
# DATA PREPROCESSING CONFIGURATION
# =============================================================================

MAX_SAMPLES = 1000
PARAMETER_FILE = 'data/raw/param/noahmp_param_sets.txt'
SIMULATION_DIR = 'data/raw/sim_results'

# Forcing file path (daily aggregate, used by the LSTM emulator).
# The site name comes from paths_config.py (one place to edit).
try:
    import paths_config as _paths_config
    _SITE_NAME = _paths_config.SITE_NAME
except ImportError:
    _SITE_NAME = 'PSO'

FORCING_FILE_DAILY = f'data/raw/forcing/forcing_{_SITE_NAME}_daily.nc'


def get_forcing_file():
    """Return the daily forcing file path used by the emulator."""
    return FORCING_FILE_DAILY


FORCING_FILE = get_forcing_file()  # For backward compatibility

OUTPUT_FILE = 'data/processed_data_forward_comprehensive.pkl'

# Parameters that should be log10-transformed before normalization
LOG_TRANSFORM_PARAMS = ['SATDK_SCL']  # Apply log10 transform during preprocessing

# Normalization method
NORMALIZATION_METHOD = 'z-score'

# =============================================================================
# MODEL CONFIGURATION
# =============================================================================

MODEL_CONFIG = {
    'model_type': 'AttentionLSTM',
    'hidden_dim': 256,
    'num_layers': 2,
    'dropout': 0.2,
    'param_embedding_dim': 64,
}

# =============================================================================
# TRAINING CONFIGURATION
# =============================================================================

TRAINING_CONFIG = {
    'learning_rate': 0.0005,
    'batch_size': 16,
    'num_epochs': 500,
    'patience': 50,
    'train_ratio': 0.64,
    'val_ratio': 0.16,
    # test_ratio is the remainder: 1 - train_ratio - val_ratio = 0.20
}

# =============================================================================
# OUTPUT LOSS WEIGHTS CONFIGURATION
# =============================================================================

OUTPUT_WEIGHTS = {
    # Energy balance variables (most critical)
    'FSA': 1.0, 'FIRA': 1.0, 'HFX': 20.0, 'LH': 20.0, 'GRDFLX': 1.0,
    # Energy components
    'SAV': 1.5, 'SAG': 1.5, 'IRC': 1.5, 'SHC': 1.5, 'EVC': 1.5,
    'IRG': 1.5, 'SHG': 1.5, 'EVG': 1.5, 'GHV': 1.5,
    # Water fluxes
    'ECAN': 2.0, 'ETRAN': 2.0, 'EDIR': 2.0,
    'UGDRNOFF_RATE': 2.0, 'SFCRNOFF_RATE': 2.0,
    # Water storage
    'SOIL_M': 20.0, 'SOIL_M_L2': 1.5, 'SOIL_M_L3': 1.5, 'SOIL_M_L4': 1.5,
    'CANLIQ': 1.0,
    # Temperature
    'SOIL_T': 1.5, 'SOIL_T_L2': 1.0, 'TG': 1.5, 'TV': 1.5, 'TRAD': 1.5,
}

def get_output_weights(variable_names):
    import numpy as np
    if OUTPUT_WEIGHTS is not None:
        weights = np.array([OUTPUT_WEIGHTS.get(name, 1.0) for name in variable_names])
        return weights
    return np.ones(len(variable_names))

# =============================================================================
# RESULTS CONFIGURATION
# =============================================================================

RESULTS_DIR = 'results_forward_comprehensive'

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_forcing_variable_names():
    """Get list of forcing variable names"""
    names = [var['name'] for var in FORCING_VARIABLES]
    if ADD_TIME_FEATURES:
        names.append('doy')
    if ADD_MONTH_FEATURE:
        for i in range(12):
            names.append(f'month_{i+1}')
    return names

def get_target_variable_names():
    """Get list of target variable names"""
    names = []
    for var in TARGET_VARIABLES:
        if 'output_name' in var:
            names.append(var['output_name'])
        else:
            names.append(var['name'])
    return names

def get_target_variable_categories():
    """Get category for each target variable"""
    return [var['category'] for var in TARGET_VARIABLES]

def get_num_forcing_variables():
    """Get total number of forcing variables"""
    num_vars = len(FORCING_VARIABLES)
    if ADD_TIME_FEATURES:
        num_vars += 1
    if ADD_MONTH_FEATURE:
        num_vars += 12
    return num_vars

def get_num_target_variables():
    """Get total number of target variables"""
    return len(TARGET_VARIABLES)

def get_variables_by_category(category):
    """Get all variables in a specific category"""
    return [var for var in TARGET_VARIABLES if var['category'] == category]

def print_config():
    """Print current configuration"""
    print("="*80)
    print("COMPREHENSIVE FORWARD MODEL CONFIGURATION")
    print("="*80)

    print(f"\nTemporal Resolution: {TEMPORAL_RESOLUTION} (LSTM/emulator side, fixed)")
    print(f"Timestep: {get_timestep_seconds()} seconds")
    print(f"Main calibration targets: {MAIN_TARGETS}")

    categories = {
        'energy_balance': 'Energy Balance (Core)',
        'energy_components': 'Energy Components (Detailed)',
        'water_fluxes': 'Water Fluxes',
        'water_storage': 'Water Storage',
        'temperature': 'Temperature'
    }

    print(f"\nTarget Variables ({len(TARGET_VARIABLES)} total):")
    for cat_key, cat_name in categories.items():
        cat_vars = get_variables_by_category(cat_key)
        if cat_vars:
            print(f"\n  {cat_name} ({len(cat_vars)} variables):")
            for i, var in enumerate(cat_vars, 1):
                layer_info = f" [Layer {var['layer']}]" if 'layer' in var else ""
                output_name = var.get('output_name', var['name'])
                print(f"    {i}. {output_name}{layer_info} - {var['description']}")

    print(f"\nForcing Variables ({len(FORCING_VARIABLES)}):")
    for i, var in enumerate(FORCING_VARIABLES, 1):
        print(f"  {i}. {var['name']} - {var['aggregation']} - {var['description']}")

    if ADD_TIME_FEATURES:
        print(f"  {len(FORCING_VARIABLES)+1}. doy - Day of year (normalized)")

    print(f"\nTotal forcing features: {get_num_forcing_variables()}")
    print(f"Total target features: {get_num_target_variables()}")

    print(f"\nTime Configuration:")
    print(f"  Full range: {TIME_RANGE_FULL['start']} to {TIME_RANGE_FULL['end']}")
    print(f"  Calibration: {TIME_RANGE_CALIBRATION['start']} to {TIME_RANGE_CALIBRATION['end']}")
    print(f"  Validation: {TIME_RANGE_VALIDATION['start']} to {TIME_RANGE_VALIDATION['end']}")

    print(f"\nData Configuration:")
    print(f"  Max samples: {MAX_SAMPLES}")
    print(f"  Parameter file: {PARAMETER_FILE}")
    print(f"  Simulation dir: {SIMULATION_DIR}")
    print(f"  Forcing file: {get_forcing_file()}")
    print(f"  Output file: {OUTPUT_FILE}")
    print(f"  Normalization method: {NORMALIZATION_METHOD}")
    if LOG_TRANSFORM_PARAMS:
        print(f"  Log10-transformed parameters: {LOG_TRANSFORM_PARAMS}")

    print(f"\nModel Configuration:")
    for key, value in MODEL_CONFIG.items():
        print(f"  {key}: {value}")

    print(f"\nTraining Configuration:")
    for key, value in TRAINING_CONFIG.items():
        print(f"  {key}: {value}")

    print("="*80)

if __name__ == '__main__':
    print_config()

# =============================================================================
# FIXED PARAMETERS CONFIGURATION (Excluded from Calibration)
# =============================================================================
# Parameters with fixed/measured values that should NOT be calibrated.
# These parameters will be set to the specified values during calibration,
# allowing the model to learn their relationship with outputs while
# calibrating only the remaining parameters.
#
# Format: {'PARAM_NAME': fixed_value}
# Example: {'LAI_EBF': 6.5} means LAI is fixed at 6.5 (e.g., from field measurement)
#
# Note: 
# - Parameter names should match those in param_names (e.g., 'LAI_EBF', 'VCMX25_EBF')
# - For SATDK, use log10-transformed value since emulator uses log transform
# - Set to empty dict {} to calibrate all parameters

# Example for a tropical evergreen broadleaf site with field-measured LAI:
#     FIXED_PARAMETERS = {'LAI_EBF': 6.52}
# Default: calibrate ALL parameters.
FIXED_PARAMETERS = {}

def get_fixed_parameters():
    """Get dictionary of fixed parameters"""
    return FIXED_PARAMETERS

def get_calibration_param_names(all_param_names):
    """
    Get list of parameter names to calibrate (excluding fixed parameters)
    
    Args:
        all_param_names: List of all parameter names
        
    Returns:
        List of parameter names that should be calibrated
    """
    fixed_params = set(FIXED_PARAMETERS.keys())
    return [name for name in all_param_names if name not in fixed_params]

def apply_fixed_parameters(params, param_names):
    """
    Apply fixed parameter values to a parameter array
    
    Args:
        params: Parameter array (will be modified in-place)
        param_names: List of parameter names corresponding to params array
        
    Returns:
        Modified parameter array with fixed values applied
    """
    import numpy as np
    params = np.array(params).copy()
    
    for param_name, fixed_value in FIXED_PARAMETERS.items():
        if param_name in param_names:
            idx = param_names.index(param_name)
            params[idx] = fixed_value
            
    return params


# =============================================================================
# HVT > HVB CONSTRAINT CONFIGURATION
# =============================================================================

# Minimum difference required between canopy top height (HVT) and canopy bottom
# height (HVB) in meters. This ensures physically valid parameter combinations.
# HVT bounds: 9-55 meters, HVB bounds: 0.1-15 meters
# Without constraint, invalid combinations like HVT=10, HVB=12 are possible.
HVT_HVB_MARGIN = 0.5

# Whether to enforce the HVT > HVB + margin constraint during optimization.
# Set to False to disable constraint enforcement (not recommended).
ENFORCE_HVT_HVB = True
