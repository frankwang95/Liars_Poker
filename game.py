#!/usr/bin/python

import random
import socket
import threading
import string 
import time
import select
import MySQLdb as sql



######### PARAMETERS ########
#############################

connAttempts = 5
wordListFile = "pokemon.csv"


networkHost = 'localhost'
networkPort = int(raw_input("Enter port:"))
networkTimeout = 500


##### Database Standards
	# usertable: uid INT NOT NULL AUTO_INCREMENT, displayname TINY TEXTNOT NULL, password TINYTEXT NOT NULL
	# gametable: gid INT NOT NULL, p1 INT NOT NULL, p2 INT NOT NULL, gamestring TEXT NOT NULL, winner INT NOT NULL
########################
databaseHost = 'localhost'
databaseUser = 'pinocchio'
databasePass = 'liarliarpantsonfire'



######### GLOBAL DEFINITONS ########
####################################

wordList = (open(wordListFile, 'r').read()).split()


def send(conn, message):
	n = connAttempts
	while n != 0:
		try:
			conn.sendall(message + "\n")
			return True
		except: n -= 1
	conn.close()
	return False


def recv(conn):
	n = connAttempts
	while n != 0:
		try:
			msg = conn.recv(1024)
			return msg.lower().strip()
		except: n -= 1

	conn.close()
	return ""


def timePrint(str):
	print time.strftime('%m-%d-%y %H:%M:%S: '), str



######### CLASSES ########
##########################

# Game Object
class Game:
	# Return Codes
	ValidChallenge = 0
	ValidTurn = 1
	InvalidInput = 2
	InsufficientBet = 3


	def __init__(self, thread1, thread2):
		self.players = [thread1, thread2]
		random.shuffle(self.players)
		self.bills = [self.generateBill(), self.generateBill()]
		self.lastBet = (0, 0)
		self.i = 0
		self.gameSummary = [self.bills[0], self.bills[1]]
		timePrint("Game started with " + str(self.players[0].uid) + " and " + str(self.players[1].uid))
		self.playGame()


	def currentPlayer(self):
		return self.players[self.i]


	def writeDatabase(self):
		db = sql.connect(db="liarspoker", host=databaseHost, user=databaseUser, passwd=databasePass)
		cur = db.cursor()
		cur.execute("INSERT INTO gametable (p1, p2, gamestring, winner) VALUES (%s, %s, %s, %s)",
			(self.players[0].uid, 
			 self.players[1].uid, 
			 ",".join([str(x) for x in self.gameSummary[:-1]]), 
			 self.gameSummary[-1].uid))
		db.commit()
		db.close()
		return


	def generateBill(self):
		billString = "%s" % (random.randint(1, 9))
		for i in range(7):
			billString += "%s" % (random.randint(0, 9))
		return billString


	def testChallenge(self):
	# Returns True if challenge fails
		if self.lastBet == (0,0):
			return True
		comb = self.bills[0] + self.bills[1]
		return comb.count(str(self.lastBet[1])) >= self.lastBet[0]


	def printGame(self):
	# Prints a summary of current step of game
		print '\n'
		print 'Player 1 Bill: ', self.bills[0]
		print 'Player 2 Bill: ', self.bills[1]
		if self.gameSummary[-2] == 'c':
			print 'Challenged: Winner: ', self.gameSummary[-1].uid
			return
		if self.currentPlayer == False:
			print 'Turn {}: Player 1'.format(len(self.gameSummary) - 1)
		else:
			print 'Turn {}: Player 2'.format(len(self.gameSummary) - 1)
		if len(self.gameSummary) > 2:
			print 'Last Move: ', self.lastBet


	def inFilter(self, inputString):
	# Returns bet tuple if valid input, else returns errorCode
		if inputString.strip().lower() == 'c':
			return 'c'
		inputString = inputString.split()
		if len(inputString) != 2:
			return self.InvalidInput
		if not all(x.isdigit() for x in inputString):
			return self.InvalidInput
		q = int(inputString[0])
		n = int(inputString[1])
		if n < 0 or n > 9:
			return self.InvalidInput
		if (q, n) <= self.lastBet:
			return self.InsufficientBet
		return (q, n)


	def turn(self, inputString):	
	# Returns 0/1 if successful, otherwise returns an error code.
		filtered = self.inFilter(inputString)
		if filtered == 'c':
			self.gameSummary.append('c')
			self.gameSummary.append(self.players[(self.i + self.testChallenge()) % 2])
			return self.ValidChallenge
		if type(filtered) == type((0, 0)):
			self.lastBet = filtered
			self.gameSummary.append("%02d %d" % filtered)
			self.i = 1 - self.i
			return self.ValidTurn
		return filtered


	def playGame(self):
		# Initial packet
		for i in [0,1]:
			send(self.players[i].conn, "Your bill: " + str(self.bills[i]) + "\nYour opponent: " + self.players[i].toHash(self.players[1 - i].uid))
		# Game in progress loop
		while self.gameSummary[-2] != 'c':
			while True:
				if send(self.currentPlayer().conn, "Last move: " + str(self.lastBet)):
					rec = recv(self.currentPlayer().conn)
					turnResult = self.turn(rec)
 					if turnResult == self.InvalidInput:
						send(self.currentPlayer().conn, "Invalid input")
						continue
					if turnResult == self.InsufficientBet:
						send(self.currentPlayer().conn, "Insufficient bet")
						continue
					break 
				else:
					self.turn('c')
					break
		# Send winners, write to database, print serverlog
		send(self.gameSummary[-1].conn, str(('w', self.bills[0], self.bills[1])))
		send(self.players[1 - ((self.i + self.testChallenge()) % 2)].conn, str(('l', self.bills[0], self.bills[1])))
		self.writeDatabase()
		timePrint("Game ended with " + str(self.players[0].uid) + " and " + str(self.players[1].uid))



# Server Object
class Server:
	def __init__(self, HOST = networkHost, PORT = networkPort):
		self.players = {}
		timePrint("Importing player list")
		self.setupPlayerList()
		timePrint("Player list imported")
		timePrint("Server Initialized")
		self.s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.s.bind((HOST, PORT))
		self.s.listen (5)
		timePrint("Listening for connections...")
		while True:
			(conn, addr) = self.s.accept()
			conn.settimeout(networkTimeout)
			timePrint("Connection made with " + str(addr))
			threading.Thread(target = Client, args = (conn, self)).start()


	def setupPlayerList(self):
		self.players = {'*':[]}
		db = sql.connect(db = "liarspoker", host = databaseHost, user = databaseUser, passwd = databasePass)
		cur = db.cursor()
		cur.execute("SELECT uid FROM usertable")
		for i in cur.fetchall():
			self.players[i[0]] = [[], []]



# Client Thread Object
class Client:
	def __init__(self, conn, server):
		self.conn = conn
		self.server = server
		if self.login():
			self.server.players[self.uid][0].append(self)
			timePrint("Signing on UID " + str(self.uid))
			self.eventTag = threading.Event()
			self.result = False
			self.gameLoop()


	def loginPullFromDB(self, usrn, pssw):
		db = sql.connect(db="liarspoker", host=databaseHost, user=databaseUser, passwd=databasePass)
		cur = db.cursor()
		cur.execute("SELECT EXISTS(SELECT 1/0 FROM usertable WHERE username=%s)", (usrn,))
		if cur.fetchall()[0][0] == 1:
				cur.execute ("SELECT password FROM usertable WHERE username=%s", (usrn,))
				if cur.fetchall()[0][0] == pssw:
						cur.execute ("SELECT uid FROM usertable WHERE username=%s", (usrn,))
						db.close()
						return cur.fetchall()[0][0]
				db.close()
				return False
		cur.execute("INSERT INTO usertable (username, password) VALUES (%s, %s)", (usrn, pssw,))
		cur.execute ("SELECT uid FROM usertable WHERE username=%s", (usrn,))
		res = cur.fetchall()
		db.commit()
		db.close()
		self.server.players[res[0][0]] = [[], [], False]
		return res[0][0]


	def checkLogin(self, str): # Formatting: 'username password'
		try:
			[username, password] = str.split()
			return [username, password]
		except:
			return False


	def close(self):
		self.conn.close()
		self.server.players[self.uid][0].remove(self)
		timePrint("Signing out UID " + str(self.uid))


	def login(self):
		while True:
			if not send (self.conn, "Login (USERNAME PASSWORD)"):
				self.conn.close
				return False
			loginInfo = self.checkLogin(recv(self.conn))
			if not loginInfo:
				send(self.conn, "Bad Input String")
				continue
			self.uid = self.loginPullFromDB(loginInfo[0], loginInfo[1])
			if not self.uid:
				send (self.conn, "Username taken or incorrect password")
				continue
			break
		return True


	def gameLoop(self):
		optionsStr = '''Commands:\n1.) M for random match\n3.) M _username_ for targeted match\n2.) V to view pending requests'''
		while True:
			if not send(self.conn, optionsStr):
				self.close()
				break
			command = recv(self.conn).split()
			if len(command) == 0: continue
			if command == ['v']:
				self.sendRequests()
				continue
			if command[0] == 'm':
				timePrint(str(self.uid) + " searching for match")
				try: req = command[1]
				except: req = '*'
				gameResult = self.match(req)
				continue
			send(self.conn, "Bad command")


	def match(self, target):
		if target == '*':
			if self.server.players[self.uid][1] == []:
				if self.server.players['*'] == []:
					self.server.players['*'].append(self)
					self.eventTag.wait()
					return self.result
				opp = self.server.players['*'].pop(0)
			else: opp = self.server.players[self.uid][1].pop(0)
		else:
			try: targetUID = self.fromHash(target)
			except: targetUID = NULL
			if targetUID not in self.server.players:
				send(self.conn, "Invalid username")
				timePrint(str(self.uid) + " match failed")
				return False
			opp = self.extract(targetUID)
			if opp == False:
				self.server.players[targetUID][1].append(self)
				self.eventTag.wait()
				return self.result
		if opp.testConn(): return self.match(target)
		gameResult = Game(self, opp)
		opp.result = gameResult
		opp.eventTag.set()
		opp.eventTag.clear()
		opp.result = False
		return gameResult


	def sendRequests(self):
		package = str(map(lambda x: self.toHash(x.uid), self.server.players[self.uid][1]))
		send(self.conn, package)



	def extract(self, uid):
		for i in self.server.players[self.uid][1]:
			if i.uid == uid:
				self.server.players[self.uid][1].remove(i)
				return i
		for i in self.server.players['*']:
			if i.uid == uid:
				self.server.players['*'].remove(i)
				return i
		return False


	def testConn(self):
	# Returns true if connection is dead
		self.conn.setblocking(0)
		self.conn.settimeout(0)
		try: status = self.conn.recv(1024)
		except: status = True
		self.conn.setblocking(1)
		self.conn.settimeout(networkTimeout)
		if status == '':
			timePrint("Signing out UID " + str(self.uid))
			return True
		return False


	def toHash(self, target_uid):
		return wordList[self.uid + target_uid]
	

	def fromHash(self, str):
		return (wordList.index(str) - self.uid)


Server()
## TO DO
# Alerts whe moving out of turn