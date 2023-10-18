from question_model import Question
from data import question_data
from quiz_brain import QuizBrain


def quiz(ques):
    question_bank_1 = []
    for i in ques:
        a = Question(i["text"], i["answer"])
        question_bank_1.append(a)  # append in dictionaries, not +

    return question_bank_1


question_bank = quiz(question_data)

game = QuizBrain(question_bank)

while game.still_has_questions():
    game.next_question()

print("You have completed the quiz.\n"
      f"Your final score is {game.score}/12")
# game is a special instance of QuizBrain and therefore use game.score
# another quiz from online site
