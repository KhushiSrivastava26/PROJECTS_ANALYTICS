# PONG GAME

# SCREEN
# SCORE
# RACKETS AND MOVE
# BALL AND MOVE
# DETECT COLLISION WITH WALL AND BOUNCE
# DETECT COLLISION WITH PADDLE
# DETECT WHEN PADDLE MISSES
# _____________________________

# Importing Necessary Libraries
from turtle import Turtle, Screen
from Paddle import Paddle
from Ball import Ball
import time
from Score import Score
import clear

# __Setting the screen__
screen = Screen()
screen.setup(width=800, height=600)
screen.bgcolor("black")
screen.title("PONG")
screen.tracer(0)

# __Game Net__
net = Turtle()
net.goto(0, 300)
net.color("white")
net.right(90)
while net.ycor() > -300:
    net.pendown()
    net.forward(10)
    net.penup()
    net.forward(10)

# __Importing components of Game__
r_paddle = Paddle((350, 0))
l_paddle = Paddle((-350, 0))
ball = Ball()
left_score = Score(-80)
right_score = Score(80)

# Setting Game keys
screen.listen()
screen.onkey(r_paddle.up, "Up")
screen.onkey(r_paddle.down, "Down")
screen.onkey(l_paddle.up, "w")
screen.onkey(l_paddle.down, "s")

# __Starting the Game__
game = True
while game:
    time.sleep(ball.ball_pace)
    screen.update()
    ball.move_ball()

    # Detect collision with wall and bounce
    if ball.ycor() > 280 or ball.ycor() < -280:
        ball.bounce_y()

    # Detect collision with right paddle
    if ball.distance(r_paddle) < 50 and ball.xcor() > 320:
        ball.bounce_x()
        right_score.clear()
        right_score.score += 1
        right_score.update()

    # Detect collision with left paddle
    if ball.distance(l_paddle) < 50 and ball.xcor() < -320:
        ball.bounce_x()
        left_score.clear()
        left_score.score += 1
        left_score.update()

    # Detect when right paddle missed the ball
    if ball.xcor() > 340:
        left_score.clear()
        left_score.score += 1
        left_score.update()
        ball.reset()

    # Detect when left paddle missed the ball
    if ball.xcor() < -340:
        right_score.clear()
        right_score.score += 1
        right_score.update()
        ball.reset()

screen.exitonclick()
