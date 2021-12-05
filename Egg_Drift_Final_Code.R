#input parameters
velocity_diff_min=0.002
velocity_diff_max=0.005
start_velocity=0.22
steps=150
cell_length=2500
inflation_time=90.00
gbi_time=floor(((rnorm(1,mean=inflation_time,sd=3))*60*60))
iterations=500

#set everything to 0 outside the loop
all_variance=0
x=0
OUTPUT=NULL

#Matrices for loop outputs 
time_matrix=matrix(nrow=steps,ncol=3)

random_velocity=matrix(nrow=steps,ncol=1)

final_cell_matrix=matrix(nrow=iterations,ncol = 1)

#Run a bunch of iterations of the model
for (p in 1:iterations){
  #input random velocities into the model
  random_velocity=matrix(nrow=steps,ncol=1)
  all_variance=0
  x=0
  for (i in 1:steps){
    x[i]=runif(1,min=velocity_diff_min,max=velocity_diff_max)
    all_variance=all_variance+x
    random_velocity[i]=start_velocity+all_variance
  }
  #random velocity output
  random_velocity[p]
  time_matrix=matrix(nrow=steps,ncol=3)
  OUTPUT=NULL
  #loop for gas bladder inflation cells 
  for (r in 1:steps){ 
    #cell length
    length=cell_length
    time_matrix[r,2]=length/random_velocity[r,]
    time_matrix[,1]=(1:steps)
    time_matrix[r,3]=sum(time_matrix[1:r,2])
    total_time=time_matrix[r,3]
    #if statement to tell loop to stop once they reach gbi stage 
    if(total_time>=gbi_time){
      #output of which cell individual reaches gbi stage in
      stage_cell = c(r, sum(time_matrix[1:r,2]))  
      OUTPUT = rbind(OUTPUT, stage_cell)
      break
    }
    OUTPUT[p]
  }
  final_cell_matrix[p]=OUTPUT[,1]
}


#matrices with cells that individual reaches gbi stage for each iteration
final_cell_matrix

#average of the final cell where eggs hatch 
target_area=mean(final_cell_matrix)
#min and max values to input into the histogram 
lowest_cell=min(final_cell_matrix)
highest_cell=max(final_cell_matrix)

#histogram of the Inflation cells from my model 
hist(final_cell_matrix, xlab = 'River Inflation Cell',main = 'Gas Bladder Inflation Cell Frequencies',breaks=lowest_cell:highest_cell,col='red',xlim = c(lowest_cell,highest_cell))

#Summary of values from the moodel
summary(final_cell_matrix)



