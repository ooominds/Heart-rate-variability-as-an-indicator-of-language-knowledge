
require(pcalg)     # Graphical Modelling
require(igraph)
require(gRim)

load('graphdat.rda')

colnames(graphdat) = c('Mean\nPulse', 'Mean\nHeartRate', 'Std.Dev.\nPulse',
    'Std.Dev.\nHeartRate', 'HeartRate\nVariability\nNN50',
    'HeartRate\nVariability\nRMSSD', 'Mean\nSkinCond.', 'Std.Dev\nSkinCond.')


########################
### Graphical Models ###
########################

### 1 ### Model with cardiovascular and electrodermal measures

n = nrow(graphdat)
V = colnames(graphdat) # node names
pc.land = pc(suffStat=list(C=cor(graphdat), n=n),
    indepTest=gaussCItest, # indep.test: partial correlations
    alpha=0.05, labels=V, u2pd='retry')

# NOTE: You can use suppressWarnings(pc(...)) to suppress the warning
#       about the change(s) in the Matrix package (dgMatrix),
#       which does not affect the modelling.

# Visualisation
gr = as(pc.land@graph, 'igraph')
gr.layout = matrix(
    c(
         0.53, -1.10, # PULSE.M
         0.58,  0.75, # HR.M
        -1.01,  1.52, # PULSE.SD
        -0.44,  0.63, # HR.SD
         1.50, -0.50, # HRV.NN50
         1.68,  0.43, # HRV.RMSSD
        -0.66, -1.46, # SC.M
        -1.27, -0.40  # SC.SD
    ),ncol=2, byrow=TRUE)
plot.igraph(gr, vertex.color='#D3D3D3', layout=gr.layout,
    vertex.shape='sphere', vertex.size=30,
    vertex.label.font=2, vertex.label.cex=1.2,
    vertex.label.color='black', edge.arrow.size=0.75)

### 2 ### Model with cardiovascular measures only

graphdat2 = graphdat[,1:6]

n2 = nrow(graphdat2)
V2 = colnames(graphdat2) # node names
pc.land2 = pc(suffStat=list(C=cor(graphdat2), n=n2),
    indepTest=gaussCItest, # indep.test: partial correlations
    alpha=0.05, labels=V2, u2pd='retry')

gr2 = as(pc.land2@graph, 'igraph')
gr2.layout = matrix(
    c(
         0.25, -0.95, # PULSE.M
         0.58,  1.10, # HR.M
        -0.95, -0.25, # PULSE.SD
        -0.44,  0.63, # HR.SD
         1.50, -0.50, # HRV.NN50
         1.85,  0.43  # HRV.RMSSD
    ),ncol=2, byrow=TRUE)
plot.igraph(gr2, vertex.color='#D3D3D3', layout=gr2.layout,
    vertex.shape='sphere', vertex.size=30,
    vertex.label.font=2, vertex.label.cex=1.2,
    vertex.label.color='black', edge.arrow.size=0.75)
