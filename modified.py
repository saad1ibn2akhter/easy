import turtle
from turtle import *
import random
from random import randrange

setup(900,900)
Screen()
turtle = turtle.Turtle()
t = turtle.Turtle()
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
def write(): 
        print(turtle.xcor())
def write2(): 
        print(turtle.ycor())

'''for i in range(100):
    array_x = [10,30,50,70,90,250,70,140,90,80,200,150,100]
    m = random.randrange(0,len(array_x)-1)
    random_x = array_x[m]

for i in range(100):
    array_y = [50,90,70,110,190,150,200,250,230,70,30,10]
    n = random.randrange(0,len(array_y)-1)
    random_y = array_y[n]
for i in range(100):
    turtle.goto(random_x,random_y)'''
array_x = [10,30,50,70,90,250,70,140,90,80,200,150,100]
m = random.randrange(0,len(array_x)-1)
random_x = array_x[m]
array_y = [50,90,70,110,190,150,200,250,230,70,30,10]
n = random.randrange(0,len(array_y)-1)
random_y = array_y[n]

t.goto(random_x,random_y)
t.circle(50)


listen()
onkey(up, 'Up')
onkey(down, 'Down')
onkey(left, 'Left')
onkey(right, 'Right')

onkey(write, 'a')
onkey(write2, 'b')


mainloop()