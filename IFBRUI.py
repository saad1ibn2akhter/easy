import turtle
t = turtle.Turtle()

t.speed(0)


t.fillcolor("yellow")
t.begin_fill()
for i in range(6):
    t.forward(100)
    t.right(60)
t.end_fill()


t.fillcolor("green")
t.begin_fill()
for i in range(3):
    t.forward(100)
    t.left(120)
t.end_fill()
t.forward(100)

t.fillcolor("green")
t.begin_fill()
for i in range(3):
    t.forward(100)
    t.left(120)
t.end_fill()
    
t.left(60)
t.right(180)

t.fillcolor("green")
t.begin_fill()
for i in range(3):
    t.forward(100)
    t.left(120)
t.end_fill()
    
t.forward(100)
t.right(120)

