from random import randint
name=""
gold=0
gender=""
balls=0
starterName=""
starterLvl=1
starterSkill1=""
starterSkill2=""
starterHp=100
enemyName=""
enemyLvl=1
enemySkill1=""
enemySkill2=""
enemyHp=100
def addEnemySkills( a,  b):
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	starterSkill1 = a

	starterSkill2 = b

	return 0
	exit()
def addSkills( a,  b):
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	starterSkill1 = a

	starterSkill2 = b

	return 0
	exit()
def useEnemySkill():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print (enemyName)

	print ("uses")

	chooseSkill = 0

	d1 = 0

	d2 = 0

	d1 = randint(1,6);
	d2 = randint(1,6);
	chooseSkill = randint(1,6);
	if chooseSkill>=3:
		print (enemySkill1)
		print (d1+d2)
		print ("\n")
		starterHp = starterHp-d1-d2
		print ("damage!")

	else:
		print (enemySkill2)
		print (d1+d2)
		print ("\n")
		starterHp = starterHp-d1-d2
		print ("damage!")

	return 0
	exit()
def main():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("Welcome to Pokeymans, the premium pet gladiator game in the world!")

	print ("What's your name?")

	name = str(raw_input());

	gold = 100

	balls = 0

	print ("Are you a boy or a girl?")

	print("\n1: Girl\n2: Boy\n")

	choice = 0

	choice = int(raw_input());

	if choice==1:
		gender = "boy"

	if choice==2:
		gender = "girl"

	else:
		gender = "undefined"

	print ("Choose your starter pokeyman!")

	print("\n1: Penusaur\n2: Blakbois\n3: Charitard\n")

	choice = int(raw_input());

	if choice==1:
		starterName = "Charitard"
		addSkills("Hot Breath", "Spicy Breath")

	if choice==2:
		starterName = "BlacBois"

	if choice==3:
		starterName = "Penusaur"

	else:
		starterName = "Potatochu"

	print ("Time to start your adventure!:")

	mainMenu()

	exit()
def mainMenu():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	while (1==1):
		print ("What would you like to do?")
		print("\n1: Leave\n2: Procrastinate!\n3: Shop!\n4: Battle!\n")
		choice = int(input("Enter a choice: "))
		while(choice!=-1):
			if (choice==1):
				leave()
			if (choice==2):
				procrastinate()
			if (choice==3):
				shop()
			if (choice==4):
				initBattle()
			else:
				choice = int(input("Invalid Input! Please Re-enter: "))


	exit()
def leave():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("Goodbye~")

	exit()
def procrastinate():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	sum = 0

	index = 1

	dieRoll = 0

	dieRoll = randint(1,6);
	print ("you roll a dice and you get a total of")

	print (dieRoll)

	mainMenu()

	exit()
def shop():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("you are in the store")

	print ("you have :")

	print (gold)

	print("\n1: Leave\n2: Buy a pokeyball (10g)\n3: buy a sandwich (20g)\n")

	choice = int(input("Enter a choice: "))
	while(choice!=-1):
		if (choice==1):
			leaveShop()
		if (choice==2):
			buyBall()
		if (choice==3):
			sandwich()
		else:
			choice = int(input("Invalid Input! Please Re-enter: "))

	exit()
def sandwich():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	rand = 0

	rand = randint(1,6);
	if gold>=20:
		gold = gold-20
		if rand>=3:
				print ("you ate a molded sandwich... Disgusting! (HP-10)")
				starterHp = starterHp-10

		else:
				print ("you ate an overpriced sandwich... The inflation these days... (HP+10)")
				starterHp = starterHp+10

	else:
		print ("you can't afford it...")

	shop()

	exit()
def buyBall():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	if gold>=10:
		print ("you buy a designer pokeyball(TM)")
		gold = gold-10
		balls = balls+1

	else:
		print ("you can't afford it...")

	shop()

	exit()
def leaveShop():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	mainMenu()

	exit()
def initBattle():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	enemyName = "ratatatatata"

	enemyLvl = starterLvl

	enemyHp = 100

	addEnemySkills("bite", "rabies")

	fight()

	exit()
def fight():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("you are fighting a")

	print (enemyName)

	print ("what will you do?")

	print("\n1: flee\n2: use pokeyball\n3: starterSkill2\n4: starterSkill1\n")

	choice = int(input("Enter a choice: "))
	while(choice!=-1):
		if (choice==1):
			flee()
		if (choice==2):
			useBall()
		if (choice==3):
			useSkill2()
		if (choice==4):
			useSkill1()
		else:
			choice = int(input("Invalid Input! Please Re-enter: "))

	useEnemySkill()

	processBattle()

	exit()
def useSkill1():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print (starterName)

	print ("uses")

	print (starterSkill1)

	d1 = 0

	d2 = 0

	d1 = randint(1,6);
	d2 = randint(1,6);
	print (d1+d2)

	print ("\n")

	enemyHp = enemyHp-d1-d2

	print ("damage!")

	exit()
def useSkill2():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print (starterName)

	print ("uses")

	print (starterSkill2)

	d1 = 0

	d2 = 0

	d1 = randint(1,6);
	d2 = randint(1,6);
	print (d1+d2)

	print ("\n")

	enemyHp = enemyHp-d1-d2

	print ("damage!")

	exit()
def processBattle():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	if enemyHp<=0:
		victory()

	if starterHp<=0:
		defeat()

	fight()

	exit()
def useBall():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	if balls>=1:
		print ("you throw the ball as hard as you can and deal critical damage to the enemy")
		balls = balls-1
		enemyHp = 0
		victory()

	else:
		print ("you don't have any balls you dunce")

	fight()

	exit()
def flee():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("only cowards flee, you fail the game...")

	exit()
def victory():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	starterHp = starterHp+30

	print ("VICTORY")

	spoils = 0

	spoils = randint(1,6);
	gold = gold+spoils*10

	mainMenu()

	exit()
def defeat():
	global enemyHp;
	global enemySkill2;
	global enemySkill1;
	global enemyLvl;
	global enemyName;
	global starterHp;
	global starterSkill2;
	global starterSkill1;
	global starterLvl;
	global starterName;
	global balls;
	global gender;
	global gold;
	global name;
	print ("You have been defeated. That was a shameful display. Slink home.")

	starterHp = 10

	mainMenu()

	exit()
if __name__ == '__main__':
	main()