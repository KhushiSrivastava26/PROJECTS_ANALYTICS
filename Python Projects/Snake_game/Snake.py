from turtle import Turtle

STARTING_POSITIONS = [(0, 0), (-20, 0), (-40, 0)]
class Snake:

    def __init__(self):
        self.xcor = 0
        self.segment = []
        self.create_snake()
        self.head = self.segment[0]

    # def make_snake(self):
    #     tim = Turtle(shape="square")
    #     tim.penup()
    #     tim.goto(x=self.xcor, y=0)
    #     tim.color("white")
    #     return tim

    def create_snake(self):
        for position in STARTING_POSITIONS:
            self.add_segment(position)
    def add_segment(self, position):
        tim = Turtle(shape="square")
        tim.penup()
        tim.goto(position)
        tim.color("white")
        self.xcor -= 20
        self.segment.append(tim)

    def extend(self):
        self.add_segment(self.segment[-1].position())
    def move(self):
        for seg_num in range(len(self.segment) - 1, 0, -1):
            new_x = self.segment[seg_num - 1].xcor()
            new_y = self.segment[seg_num - 1].ycor()
            self.segment[seg_num].goto(new_x, new_y)
        self.head.forward(20)


    def up(self):
        if self.head.heading() != 270:
            self.head.setheading(90)

    def down(self):
        if self.head.heading() != 90:
            self.head.setheading(270)

    def right(self):
        if self.head.heading() != 180:
            self.head.setheading(0)

    def left(self):
        if self.head.heading() != 0:
            self.head.setheading(180)

