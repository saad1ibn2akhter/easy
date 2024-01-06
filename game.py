#step 1:modules + default values

import turtle 
import time
import random

delay = 0.1
score = 0
high_score = 0

#step 2 :window + head(snake) + food + 
wn =  turtle.Screen()
wn.title("snake xengia")
wn.bgcolor("skyblue")

wn.setup(width = 600, height = 600)
wn.tracer(0)# IDK

head = turtle.Turtle()
head.shape("square")
head.color("white")
head.penup()
head.goto(0,0)
head.direction =  "Stop"

food =  turtle.Turtle()
colors = random.choice(['red','green','black'])
shapes = random.choice(['square','triangle','circle'])
food.shape(shapes)
food.color(colors)
food.penup()
food.goto(0,100)

pen = turtle.Turtle()
pen.speed(0)
pen.shape("square")
pen.color("white")
pen.penup()
pen.hideturtle()
pen.goto(0, 250)
pen.write("Score : 0  High Score : 0", align="center",
          font=("candara", 24, "bold"))



# Functions
def go_up():
    if head.direction != "down":
        head.direction = "up"

def go_down():
    if head.direction != "up":
        head.direction = "down"

def go_left():
    if head.direction != "right":
        head.direction = "left"

def go_right():
    if head.direction != "left":
        head.direction = "right"

def move():
    if head.direction == "up":
        y = head.ycor()
        head.sety(y + 20)

    if head.direction == "down":
        y = head.ycor()
        head.sety(y - 20)

    if head.direction == "left":
        x = head.xcor()
        head.setx(x - 20)

    if head.direction == "right":
        x = head.xcor()
        head.setx(x + 20)
# Keyboard bindings

wn.listen()
wn.onkeypress(go_up, "Up")
wn.onkeypress(go_down, "Down")
wn.onkeypress(go_left, "Left")
wn.onkeypress(go_right, "Right")

# Main game loop
while True:
    wn.update()

    # Check for collision with wall
    if head.xcor() > 290 or head.xcor() < -290 or head.ycor() > 290 or head.ycor() < -290:
        head.goto(0,0)
        head.write("GaMe OvEr",font=("candara", 24, "bold"))
        break

    move()
    time.sleep(delay)

wn.mainloop()


