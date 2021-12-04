Purpose: 
My first field season was unsuccessful at sampling age-0 Bighead and Silver Carp (hereafter Carp). Carp are notoriously difficult to sample and it has become necessary to come up with novel ways to attempt to sample them. One such way to improve sampling efforts can be accomplished through an agent-based model (ABM) framework. I can improve site selection by simulating drift and development of Carp eggs up until the gas bladder inflation (GBI) stage (when they begin to actively seek out nursery habitat). I can theoretically determine the distance within a specific river, in this case the Red River of Oklahoma at which Carp develop the ability to seek nursery habitat. 

State Variables and scales:
This model uses virtual agents, in this case Bighead and Silver Carp eggs simulated in simple fluvial downstream landscape. Because I am looking for the highest survival scenario, I simulated a single egg as a surrogate for the most likely survival case. The agent was given developmental qualities of age (seconds), and location within the landscape (which cell and position within) shown as meters downstream. Each cell was given an accumulative variable velocity value simulated through a uniform distribution. Because temperature values do not heavily fluctuate across locations of the same river, I assigned a uniform temperature to all cells. This was simulated by assigning a gas bladder inflation time, which is a function of temperature (i.e., at 22 °C gas bladder inflation time would be at 90 hours). The landscape was composed of a series of one-dimensional cells. For the purpose of this project, we will describe cells only by their length. Flow velocity measurements included in the cells were simulated, as we do not have the field data at this time. 

Process Overview and scheduling:
Processes included in the model were downstream movement, spawning (or egg initialization), and development of the eggs. The egg release was simulated from the first cell, which in this case will function as the location of a dam where Carp have been documented in large numbers during spawning season.  The eggs were then transported through a series of time steps across the one-dimensional landscape. The movements occurred as a function of the flow velocity values given to each individual cell. As the eggs moved through these time steps, they return a cumulative development time. They model terminates once the eggs development time reaches that of the temperature determined gas bladder inflation time. 

Design Concepts:
A one-dimensional landscape was created following the downstream flow of a river. The landscape was constructed of linearly spaced cells where movement through each cell is achieved through a series of time steps. We assigned the starting point within the landscape, which functions as the spawning location. In this landscape, I did not model the sinking or non-viability of eggs due to water column dispersion.  Instead, I gave the egg a constant survival rate to discern where they land at the point of gas bladder inflation. This simulated the best-case scenario of the egg surviving and reaching the point of development when they seek out nursery habitat. The model ends once the egg/individual reaches a time value equal or greater than that of the temperature assigned GBI time. At the end of the model, the cell in which the individual reaches the GBI stage is returned. This process was repeated at a user-defined number of iterations to calculate an average cell of inflation. The average cell of inflation was then used to determine the total longitudinal length required to reach this stage at a specific temperature. 

Initialization:
The model is initialized with the simulation of a spawning event where an egg is placed at the highest upstream point in the landscape. The egg then progresses through the cells as it develops. The model is run across a number of iterations to assess the variability in temperature and flow velocities. For this model the process is repeated 500 times. 

Input: 
Input variables included in the model were the number of cells, length of the cells, flow velocity, water temperature (through GBI inflation time), and number of iterations run, and variability a flow velocity. These variables were roughly calculated through known stream gauge data then transformed and simulated. 
