rock = '''
    _______
---'   ____)
      (_____)
      (_____)
      (____)
---.__(___)
'''

paper = '''
    _______
---'   ____)____
          ______)
          _______)
         _______)
---.__________)
'''

scissors = '''
    _______
---'   ____)____
          ______)
       __________)
      (____)
---.__(___)
'''

# Input from player
answer = input("What do you choose? Type 0 for Rock, 1 for Paper or 2 for Scissors: ")
answer = int(answer)

# User Input
if answer == 0:
    print(rock)
elif answer == 1:
    print(paper)
else:
    print(scissors)

# Computer Input
print("Computer chose:")
moves = [rock, scissors, paper]

import random

computer_choice = random.choice(moves)
print(computer_choice)
if answer < 3:
    if answer == 0 and computer_choice == rock:
        print("Tie")
    elif answer == 0 and computer_choice == scissors:
        print("You win")
    elif answer == 1 and computer_choice == rock:
        print(" You win")
    elif answer == 2 and computer_choice == paper:
        print(" You win")
    elif computer_choice == rock and answer == 2:
        print("You lose")
    elif computer_choice == paper and answer == 0:
        print("You lose")
    elif computer_choice == scissors and answer == 1:
        print("You lose")
    elif answer == 1 and computer_choice == paper:
        print("Tie")
    else:
        print("Tie")
else:
    print("You typed an invalid number")
