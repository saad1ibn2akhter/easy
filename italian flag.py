import turtle as t

t.penup()
t.left(90)
t.forward(200)
t.right(90)
t.back(300)



t.color('green')
t.begin_fill()
for i in range(2):
    t.forward(100)
    t.right(90)
    t.forward(200)
    t.right(90)
t.end_fill()
t.forward(100)

t.color('white')
t.begin_fill()
for i in range(2):
    t.forward(100)
    t.right(90)
    t.forward(200)
    t.right(90)
t.end_fill()
t.forward(100)

t.color('red')
t.begin_fill()
for i in range(2):
    t.forward(100)
    t.right(90)
    t.forward(200)
    t.right(90)
t.end_fill()
t.forward(100)
t.right(90)
t.forward(250)
t.right(90)
t.forward(200)


t.done()

