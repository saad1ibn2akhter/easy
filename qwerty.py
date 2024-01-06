import turtle
from turtle import *

setup(500, 500)
Screen()
turtle = turtle.Turtle()
turtle.speed(0)
showturtle()
#turtle.pensize(10)

def upright():
	turtle.setheading(45)#north
	turtle.forward(75)
def down():
	turtle.setheading(270)#south
	turtle.forward(50)

def upleft():
	turtle.setheading(135)#west
	turtle.forward(75)

def right():
	turtle.setheading(0)#east
	turtle.forward(50)
def left():
	turtle.setheading(180)#west
	turtle.forward(50)
def up():
	turtle.setheading(90)#west
	turtle.forward(50)
	

# loop for pattern
for i in range(4):
    turtle.forward(100)
     
    # get heading value
    val = turtle.heading()
     
    # write it
    turtle.write(str(val))
    turtle.backward(100)
    turtle.left(90)



listen()
onkey(upright, 'r')
onkey(up,'Up')
onkey(down, 'Down')
onkey(upleft, 'l')
onkey(right, 'Right')
onkey(left, 'Left')


mainloop()
