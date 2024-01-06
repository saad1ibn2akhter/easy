import turtle
p1 = turtle.Turtle()
p2 = turtle.Turtle()
p3 = turtle.Turtle()


p2.penup()
p3.penup()


p1.fillcolor("white")
p1.begin_fill()

for i in range(2):
    p1.forward(300)
    p1.right(90)
    p1.forward(180)
    p1.right(90)
p1.end_fill()

p2.right(90)
p2.forward(75)
p2.left(90)
p2.fillcolor("blue")
p2.begin_fill()
for i in range(2):
    p2.forward(300)
    p2.right(90)
    p2.forward(40)
    p2.right(90)
p2.end_fill()

p3.forward(75)
p3.right(90)
p3.fillcolor("blue")
p3.begin_fill()

for i in range(2):
    p3.forward(180)
    p3.left(90)
    p3.forward(40)
    p3.left(90)
p3.end_fill()


p1.hideturtle()
p2.hideturtle()
p3.hideturtle()


