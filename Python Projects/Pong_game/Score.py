from turtle import Turtle
import clear


# Scoreboard of the Game
class Score(Turtle):

    def __init__(self, position):
        super().__init__()
        self.color("white")
        self.penup()
        self.hideturtle()
        self.goto(position, 200)
        self.score = 0
        self.update()

    def update(self):
        self.write(self.score,
                   False,
                   align="center",
                   font=('Courier', 70, 'normal'))
