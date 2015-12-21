botDrawerLooted=0
topDrawerLooted=0
haveCigs=0
haveLighter=0
havePoison=0
triedButton=0
def main():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	start()

	exit()
def start():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	print ("You wake up in a derelict room with no memory of last night")

	print ("You shiver as the biting cold from a dripping air conditioner assaults your bare skin")

	while (havePoison==0):
		if havePoison==0:
				if havePoison==0:
						if havePoison==0:
								havePoison = 0


	room()

	exit()
def room():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	if havePoison!=1:
		print ("You spot a steel door to your left, a nightstand on your right, and a large red button on the ceiling.")
		print("\n1: Press the button\n2: Check the nightstand\n3: Try the door\n")
		choice = int(input("Enter a choice: "))
		while(choice!=-1):
			if (choice==1):
				button1()
			if (choice==2):
				nightstand1()
			if (choice==3):
				door1()
			else:
				choice = int(input("Invalid Input! Please Re-enter: "))

	else:
		print ("You spot a steel door to your left, a nightstand on your right, and a large red button on the ceiling.")
		print("\n1: Consume the contents of the jar\n2: Press the button\n3: Check the nightstand\n4: Try the door\n")
		choice = int(input("Enter a choice: "))
		while(choice!=-1):
			if (choice==1):
				die()
			if (choice==2):
				button1()
			if (choice==3):
				nightstand1()
			if (choice==4):
				door1()
			else:
				choice = int(input("Invalid Input! Please Re-enter: "))

	exit()
def door1():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	print ("You grab the handle firmly and push, but the door stays shut")

	room()

	exit()
def nightstand1():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	print ("A curiously ornate nightstand, with gilded etchings lining the side")

	print ("Two drawers present themselves")

	print("\n1: Step away\n2: Check the bottom drawer\n3: Check the top drawer\n")

	choice = int(input("Enter a choice: "))
	while(choice!=-1):
		if (choice==1):
			room()
		if (choice==2):
			bottomdrawer()
		if (choice==3):
			topdrawer()
		else:
			choice = int(input("Invalid Input! Please Re-enter: "))

	exit()
def topdrawer():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	if topDrawerLooted==0:
		print ("You find a lighter and a pack of cigarettes")
		topDrawerLooted = 1
		havLighter = 1
		haveCigs = 1

	else:
		print ("There's nothing in here")

	nightstand1()

	exit()
def bottomdrawer():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	if botDrawerLooted==0:
		print ("You find a large instect that promptly crawls away")
		print ("Underneath is a jar with mysterious contents")
		botDrawerLooted = 1
		havePoison = 1

	else:
		print ("There's nothing in here")

	nightstand1()

	exit()
def button1():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	if triedButton==1:
		button2()

	print ("You stare at the button apprehensively, then tender a hesitant poke")

	print ("Nothing really happens")

	print("\n1: Try again\n2: Go back\n")

	triedButton = 1

	choice = int(input("Enter a choice: "))
	while(choice!=-1):
		if (choice==1):
			button1()
		if (choice==2):
			room()
		else:
			choice = int(input("Invalid Input! Please Re-enter: "))

	exit()
def button2():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	if triedButton==2:
		button3()

	print ("You press the button again")

	print ("Nothing really happens")

	print("\n1: Try again\n2: Go back\n")

	triedButton = 2

	choice = int(input("Enter a choice: "))
	while(choice!=-1):
		if (choice==1):
			button2()
		if (choice==2):
			room()
		else:
			choice = int(input("Invalid Input! Please Re-enter: "))

	exit()
def button3():
	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	print ("You press the button again, with the force of a thousand men")

	print ("Suddenly, POOT, the button sinks into its groove")

	print ("Curious as to what happens? To be continued...")

	exit()
def die(): 

	global triedButton;
	global havePoison;
	global haveLighter;
	global haveCigs;
	global topDrawerLooted;
	global botDrawerLooted;
	print ("Regret flashes through your mind as the smell of almonds and the taste of arsenic fills your senses")

	print ("You died.")

	exit()
if __name__ == '__main__':
	main()