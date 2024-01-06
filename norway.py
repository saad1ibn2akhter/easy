import turtle
t = turtle.Turtle()
t2 = turtle.Turtle()

t.penup()
t2.penup()

t.fillcolor('red')
t.begin_fill()
for i in range(2):
    t.forward(300)
    t.right(90)
    t.forward(180)
    t.right(90)
t.end_fill()

t.forward(100)
t.right(90)
t.fillcolor('white')
t.begin_fill()
for i in range(2):
    t.forward(180)
    t.right(90)
    t.forward(60)
    t.right(90)
t.end_fill()

t2.right(90)
t2.forward(75)
t2.left(90)
t2.fillcolor('white')
t2.begin_fill()
for i in range(2):
    t2.forward(300)
    t2.right(90)
    t2.forward(60)
    t2.right(90)
t2.end_fill()

t.right(90)
t.forward(15)
t.left(90)
t.fillcolor('darkblue')
t.begin_fill()
for i in range(2):
    t.forward(180)
    t.right(90)
    t.forward(33)
    t.right(90)
t.end_fill()

t2.right(90)
t2.forward(15)
t2.left(90)
t2.fillcolor('darkblue')
t2.begin_fill()
for i in range(2):
    t2.forward(300)
    t2.right(90)
    t2.forward(33)
    t2.right(90)
t2.end_fill()






















