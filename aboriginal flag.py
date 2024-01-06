import turtle
t = turtle.Turtle()
c = turtle.Turtle()
t.penup()
c.penup()

t.fillcolor("yellow")
t.begin_fill()
for i in range(2):
    t.forward(300)
    t.right(90)
    t.forward(90)
    t.right(90)
t.end_fill()

t.right(90)
t.forward(90)
t.left(90)

t.fillcolor("blue")
t.begin_fill()
for i in range(2):
    t.forward(300)
    t.right(90)
    t.forward(90)
    t.right(90)
t.end_fill()


c.forward(100)
c.right(90)
c.forward(90)
c.fillcolor("red")
c.begin_fill()
c.circle(50)
c.end_fill()


t.hideturtle()
c.hideturtle()
