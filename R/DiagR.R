library(DiagrammeR)

# Flowchart  ----
output$salt_tool_flowchartA <- 
  DiagrammeR::renderDiagrammeR(
    mermaid("
graph TB
  A(KIK-IRPA database: sample info & IC data)-->B(Total salt content)
  A-->C(Ion balance)
  B-->D(<0.8% wt%)
  B-->E(>0.8% wt%)
  D-->F(Content too low)
  F-->G(STOP)
  E-->H(Balance correction, analytical error)
  C-->I(<1mEq/g)
  C-->J(>1mEq/g)
  J-->K(Measurement fault)
  K-->L(STOP)
  J-->M(Related to carbonates)
  I-->H
  M-->H
  H-->N(>2%)
  H-->O(<2%)
  O-->P(Correct all ions equally)
  N-->Q(Detect ion excess starting with Ca2+ - removed from total)
  Q-->R(Main calculations)
  Q-->S(calculate gypsum content equimolar Ca2+ + SO42- - removed from total)
  P-->S
  S-->T(Define group: Ca2+ or SO42- > 0)
"))

DiagrammeR::grViz("digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
data2 [label = 'Dataset 2', shape = folder, fillcolor = Beige]
process [label =  'Process \n Data']
statistical [label = 'Statistical \n Analysis']
results [label= 'Results']

# edge definitions with the node IDs
{data1 data2}  -> process -> statistical -> results
}")


DiagrammeR::grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]

  # several 'node' statements
  node [shape = box,
        fontname = Helvetica]
  A; B; C; D; E; F

  node [shape = circle,
        fixedsize = true,
        width = 0.9] // sets as circles
  1; 2; 3; 4; 5; 6; 7; 8

  # several 'edge' statements
  A->1 B->2 B->3 B->4 C->A
  1->D E->A 2->4 1->5 1->F
  E->6 4->6 5->7 6->7 3->8
}
")


mermaid("
graph LR
A(Rounded)-->B[Rectangular]
B-->C{A Rhombus}
C-->D[Rectangle One]
C-->E[Rectangle Two]
")

mermaid("graph LR; A-->B; A-->C; C-->E; B-->D; C-->D; D-->F; E-->F")

mermaid("
graph TB
  A-->B
  A-->C
  C-->E
  B-->D
  C-->D
  D-->F
  E-->F
")

mermaid("
graph TB
  A(KIK-IRPA database: sample info & IC data)-->B(Total salt content)
  A-->C(Ion balance)
  B-->D(<0.8% wt%)
  B-->E(>0.8% wt%)
  D-->F(Content too low)
  F-->G(STOP)
  E-->H(Balance correction, analytical error)
  C-->I(<1mEq/g)
  C-->J(>1mEq/g)
  J-->K(Measurement fault)
  K-->L(STOP)
  J-->M(Related to carbonates)
  I-->H
  M-->H
  H-->N(>2%)
  H-->O(<2%)
  O-->P(Correct all ions equally)
  N-->Q(Detect ion excess starting with Ca2+ - removed from total)
  Q-->R(Main calculations)
  Q-->S(calculate gypsum content equimolar Ca2+ + SO42- - removed from total)
  P-->S
  S-->T(Define group: Ca2+ or SO42- > 0)
")