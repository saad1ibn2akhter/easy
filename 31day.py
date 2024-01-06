import turtle
t = turtle.Turtle()
t.penup()
t.fillcolor('green')
t.begin_fill()
for i in range(3):
    t.forward(100)
    t.left(120)
t.end_fill()

t.forward(120)
'''ucla'''


t.fillcolor('red')
t.begin_fill()
for i in range(3):
    t.forward(100)
    t.left(120)
t.end_fill()

t.right(180)
t.forward(60)
t.left(90)

t.forward(50)
t.fillcolor("blue")
t.begin_fill()
t.circle(50)
t.end_fill()
t.hideturtle()
