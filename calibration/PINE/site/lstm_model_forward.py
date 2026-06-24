"""
LSTM model for FORWARD prediction
Predicts time series results from parameters and forcing data

Architecture:
- Parameter embedding layer
- LSTM to process forcing time series + parameter embeddings
- Output layer to predict target variables at each timestep
"""

import torch
import torch.nn as nn


class LSTMForwardPredictor(nn.Module):
    """
    LSTM-based model to predict time series from parameters and forcing

    Architecture:
    - Embed parameters into fixed-size representation
    - Concatenate parameter embedding with forcing at each timestep
    - LSTM processes the combined input
    - Output layer predicts target variables at each timestep
    """

    def __init__(self, n_params, n_forcing_vars, n_target_vars,
                 hidden_dim, num_layers, param_embedding_dim=64, dropout=0.2):
        """
        Args:
            n_params: Number of input parameters
            n_forcing_vars: Number of forcing variables (time series)
            n_target_vars: Number of target variables to predict
            hidden_dim: Number of hidden units in LSTM
            num_layers: Number of LSTM layers
            param_embedding_dim: Dimension for parameter embedding
            dropout: Dropout rate
        """
        super(LSTMForwardPredictor, self).__init__()

        self.n_params = n_params
        self.n_forcing_vars = n_forcing_vars
        self.n_target_vars = n_target_vars
        self.hidden_dim = hidden_dim
        self.num_layers = num_layers
        self.param_embedding_dim = param_embedding_dim

        # Parameter embedding layer
        self.param_embedding = nn.Sequential(
            nn.Linear(n_params, param_embedding_dim * 2),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(param_embedding_dim * 2, param_embedding_dim),
            nn.ReLU()
        )

        # LSTM input dimension = forcing vars + parameter embedding
        lstm_input_dim = n_forcing_vars + param_embedding_dim

        # LSTM layer
        self.lstm = nn.LSTM(
            input_size=lstm_input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout if num_layers > 1 else 0
        )

        # Output layer
        self.output_layer = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim // 2),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim // 2, n_target_vars)
        )

    def forward(self, params, forcing):
        """
        Forward pass

        Args:
            params: Parameter tensor of shape (batch_size, n_params)
            forcing: Forcing tensor of shape (batch_size, seq_len, n_forcing_vars)

        Returns:
            Output tensor of shape (batch_size, seq_len, n_target_vars)
        """
        batch_size, seq_len, _ = forcing.shape

        # Embed parameters
        # param_emb: (batch_size, param_embedding_dim)
        param_emb = self.param_embedding(params)

        # Expand parameter embedding to all timesteps
        # param_emb_expanded: (batch_size, seq_len, param_embedding_dim)
        param_emb_expanded = param_emb.unsqueeze(1).expand(-1, seq_len, -1)

        # Concatenate forcing and parameter embedding
        # lstm_input: (batch_size, seq_len, n_forcing_vars + param_embedding_dim)
        lstm_input = torch.cat([forcing, param_emb_expanded], dim=2)

        # LSTM forward pass
        # lstm_out: (batch_size, seq_len, hidden_dim)
        lstm_out, _ = self.lstm(lstm_input)

        # Apply output layer to each timestep
        # output: (batch_size, seq_len, n_target_vars)
        output = self.output_layer(lstm_out)

        return output


class BiLSTMForwardPredictor(nn.Module):
    """
    Bidirectional LSTM-based model for forward prediction
    """

    def __init__(self, n_params, n_forcing_vars, n_target_vars,
                 hidden_dim, num_layers, param_embedding_dim=64, dropout=0.2):
        """
        Args:
            n_params: Number of input parameters
            n_forcing_vars: Number of forcing variables (time series)
            n_target_vars: Number of target variables to predict
            hidden_dim: Number of hidden units in LSTM (per direction)
            num_layers: Number of LSTM layers
            param_embedding_dim: Dimension for parameter embedding
            dropout: Dropout rate
        """
        super(BiLSTMForwardPredictor, self).__init__()

        self.n_params = n_params
        self.n_forcing_vars = n_forcing_vars
        self.n_target_vars = n_target_vars
        self.hidden_dim = hidden_dim
        self.num_layers = num_layers
        self.param_embedding_dim = param_embedding_dim

        # Parameter embedding layer
        self.param_embedding = nn.Sequential(
            nn.Linear(n_params, param_embedding_dim * 2),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(param_embedding_dim * 2, param_embedding_dim),
            nn.ReLU()
        )

        # LSTM input dimension
        lstm_input_dim = n_forcing_vars + param_embedding_dim

        # Bidirectional LSTM
        self.lstm = nn.LSTM(
            input_size=lstm_input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout if num_layers > 1 else 0,
            bidirectional=True
        )

        # Output layer (input is 2*hidden_dim due to bidirectional)
        self.output_layer = nn.Sequential(
            nn.Linear(hidden_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, n_target_vars)
        )

    def forward(self, params, forcing):
        """
        Forward pass

        Args:
            params: Parameter tensor of shape (batch_size, n_params)
            forcing: Forcing tensor of shape (batch_size, seq_len, n_forcing_vars)

        Returns:
            Output tensor of shape (batch_size, seq_len, n_target_vars)
        """
        batch_size, seq_len, _ = forcing.shape

        # Embed parameters
        param_emb = self.param_embedding(params)

        # Expand to all timesteps
        param_emb_expanded = param_emb.unsqueeze(1).expand(-1, seq_len, -1)

        # Concatenate forcing and parameter embedding
        lstm_input = torch.cat([forcing, param_emb_expanded], dim=2)

        # LSTM forward pass
        lstm_out, _ = self.lstm(lstm_input)

        # Apply output layer to each timestep
        output = self.output_layer(lstm_out)

        return output


class AttentionLSTMForwardPredictor(nn.Module):
    """
    LSTM with attention mechanism for forward prediction

    The attention mechanism learns to focus on the most relevant timesteps
    when generating predictions at each timestep.
    """

    def __init__(self, n_params, n_forcing_vars, n_target_vars,
                 hidden_dim, num_layers, param_embedding_dim=64, dropout=0.2):
        """
        Args:
            n_params: Number of input parameters
            n_forcing_vars: Number of forcing variables (time series)
            n_target_vars: Number of target variables to predict
            hidden_dim: Number of hidden units in LSTM
            num_layers: Number of LSTM layers
            param_embedding_dim: Dimension for parameter embedding
            dropout: Dropout rate
        """
        super(AttentionLSTMForwardPredictor, self).__init__()

        self.n_params = n_params
        self.n_forcing_vars = n_forcing_vars
        self.n_target_vars = n_target_vars
        self.hidden_dim = hidden_dim
        self.num_layers = num_layers
        self.param_embedding_dim = param_embedding_dim

        # Parameter embedding layer
        self.param_embedding = nn.Sequential(
            nn.Linear(n_params, param_embedding_dim * 2),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(param_embedding_dim * 2, param_embedding_dim),
            nn.ReLU()
        )

        # LSTM input dimension
        lstm_input_dim = n_forcing_vars + param_embedding_dim

        # LSTM layer
        self.lstm = nn.LSTM(
            input_size=lstm_input_dim,
            hidden_size=hidden_dim,
            num_layers=num_layers,
            batch_first=True,
            dropout=dropout if num_layers > 1 else 0
        )

        # Attention mechanism for each output timestep
        self.attention = nn.Sequential(
            nn.Linear(hidden_dim, hidden_dim // 2),
            nn.Tanh(),
            nn.Linear(hidden_dim // 2, 1)
        )

        # Output layer (combines attention-weighted context with current hidden state)
        self.output_layer = nn.Sequential(
            nn.Linear(hidden_dim * 2, hidden_dim),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(hidden_dim, n_target_vars)
        )

    def forward(self, params, forcing):
        """
        Forward pass with attention mechanism

        Args:
            params: Parameter tensor of shape (batch_size, n_params)
            forcing: Forcing tensor of shape (batch_size, seq_len, n_forcing_vars)

        Returns:
            Output tensor of shape (batch_size, seq_len, n_target_vars)
        """
        batch_size, seq_len, _ = forcing.shape

        # Embed parameters
        param_emb = self.param_embedding(params)

        # Expand to all timesteps
        param_emb_expanded = param_emb.unsqueeze(1).expand(-1, seq_len, -1)

        # Concatenate forcing and parameter embedding
        lstm_input = torch.cat([forcing, param_emb_expanded], dim=2)

        # LSTM forward pass
        # lstm_out: (batch_size, seq_len, hidden_dim)
        lstm_out, _ = self.lstm(lstm_input)

        # Compute attention scores for each timestep
        # attention_scores: (batch_size, seq_len, 1)
        attention_scores = self.attention(lstm_out)

        # Apply softmax to get attention weights
        attention_weights = torch.softmax(attention_scores, dim=1)

        # Compute context vector as weighted sum of LSTM outputs
        # context: (batch_size, 1, hidden_dim)
        context = torch.sum(attention_weights * lstm_out, dim=1, keepdim=True)

        # Expand context to all timesteps
        # context_expanded: (batch_size, seq_len, hidden_dim)
        context_expanded = context.expand(-1, seq_len, -1)

        # Concatenate LSTM output with context
        # combined: (batch_size, seq_len, hidden_dim * 2)
        combined = torch.cat([lstm_out, context_expanded], dim=2)

        # Apply output layer to each timestep
        output = self.output_layer(combined)

        return output
