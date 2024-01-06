import turtle
from turtle import *
import random
from random import randrange

setup(900,900)
Screen()
turtle = turtle.Turtle()
turtle.speed(0)
showturtle()


def up():
	turtle.setheading(90)
	turtle.forward(50)




def down():
	turtle.setheading(270)
	turtle.forward(50)


def left():
	turtle.setheading(180)
	turtle.forward(50)


def right():
	turtle.setheading(0)
	turtle.forward(50)

def upright():
	turtle.setheading(45)
	turtle.forward(75)

def upleft():
	turtle.setheading(135)
	turtle.forward(75)
	
def shape():
    turtle.shape("turtle")
def square():
    colors = ["red", "blue", "green", "yellow", "orange","skyblue"]
    random_index = random.randint(0, len(colors) - 1)
    random_color = colors[random_index]
    turtle.fillcolor(random_color)
    turtle.begin_fill()
    for i in range(4):
        turtle.forward(50)
        turtle.right(90)
    turtle.end_fill()
def trinagle():
    colors = ["red", "blue", "green", "yellow", "orange"]
    random_index = random.randint(0, len(colors) - 1)
    random_color = colors[random_index]
    turtle.fillcolor(random_color)
    turtle.begin_fill()
    for i in range(3):
        turtle.forward(50)
        turtle.right(120)
    turtle.end_fill()

def circle():

    colors = ["red", "blue", "green", "yellow", "orange"]
    random_index = random.randint(0, len(colors) - 1)
    random_color = colors[random_index]
    turtle.fillcolor(random_color)
    turtle.begin_fill()
    turtle.circle(25)
    turtle.end_fill()

def pup():
    turtle.penup()
def pdown():
    turtle.pendown()
    
def color():
    colors = ["red", "blue", "green", "yellow", "orange"]
    random_index = random.randint(0, len(colors) - 1)
    random_color = colors[random_index]
    turtle.pencolor(random_color)
def pensize():
    turtle.pensize(10)
def pensize2():
    turtle.pensize(1)

def write(): 
        print(turtle.xcor())
def write2(): 
        print(turtle.ycor())
def erase():
        turtle.clear()


listen()
onkey(up, 'Up')
onkey(down, 'Down')
onkey(left, 'Left')
onkey(right, 'Right')
onkey(shape, 's')
onkey(square, 'q')
onkey(trinagle, 'w')
onkey(pup, 'p')
onkey(pdown, 'd')
onkey(color, 'c')
onkey(pensize, 'm')
onkey(pensize2, 'n')
onkey(write, 'a')
onkey(write2, 'b')
onkey(upleft, 'l')
onkey(upright, 'r')
onkey(circle, 'g')
onkey(erase, 'x')

mainloop()
