## For plotting experiment 2
require(gridExtra)
plt.mem = plt.mem + scale_y_continuous(labels = scales::percent)+ ylim(0, 1) 
grid.arrange(plt.mem, plt.value, ncol=2)



## Data reporting experiment 2

m.v.chocie = 