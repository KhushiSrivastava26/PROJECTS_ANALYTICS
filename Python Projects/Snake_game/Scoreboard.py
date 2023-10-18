from turtle import Turtle


class Score(Turtle):
    def __init__(self):
        super().__init__()
        self.color("white")
        self.penup()
        self.hideturtle()
        self.goto(0,260)
        self.score = 0
        self.update()
    def update(self):
        self.write(f"Score : {self.score}",
                   False,
                   align="center",
                   font=('Arial', 24, 'normal'))

    def game_over(self):
        self.goto(0,0)
        self.write("GAME OVER.",
                   False,
                   align="center",
                   font=('Arial', 24, 'normal'))
    def cal_score(self):
        self.clear()
        self.score += 1
        self.update()
