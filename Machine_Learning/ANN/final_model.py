import torch
from torch import nn, optim
import torch.nn.functional as F

# develop model
class SoftmaxClassifier(nn.Module):
    def __init__(self, input_size):
        super().__init__()
        self.hidden1 = nn.Linear(input_size, 100)
        self.hidden2 = nn.Linear(100,100)
        self.hidden3 = nn.Linear(100, 100)
        self.hidden4 = nn.Linear(100, 50)
        self.output = nn.Linear(50,2)
        self.dropout = nn.Dropout(p=0.1)
        
    def forward(self, x):
        z = self.dropout(F.relu(self.hidden1(x)))
        z = self.dropout(F.relu(self.hidden2(z)))
        z = self.dropout(F.relu(self.hidden3(z)))
        z = self.dropout(F.relu(self.hidden4(z)))
        out = F.log_softmax(self.output(z), dim=1)
        
        return out