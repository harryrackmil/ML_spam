stops = open("http://www.textfixer.com/resources/common-english-words.txt", 'r')
stopwords = stops.read().split(",")
stops.close()

text_train = open(r'C:\Users\Harry\Documents\Fall 14\Stat 154\train_msgs.txt', encoding='utf-8')

label = []
allwords = set()
wordsbytext = []
for line in text_train:
	tabsplit = line.split("\t")
	label.append(tabsplit[0])
	words = tabsplit[1][:-1].split()
	wordsbytext.append({});
	textlen = len(words)
	for w in words:
		if w not in stopwords:
			allwords.add(w)
			if w in wordsbytext[-1]:
				wordsbytext[-1][w] += 1/textlen
			else:
				wordsbytext[-1][w] = 1/textlen
	if wordsbytext[-1] == {}:
		wordsbytext.pop()

allwords = list(allwords)
countmat = []
for text in wordsbytext:
	countmat.append([])
	for word in allwords:
		if word not in text:
			countmat[-1].append(0)
		else:
			countmat[-1].append(text[word])


import csv

with open("raw_mtx.csv", "wb") as f:
    writer = csv.writer(f)
    writer.writerows(countmat)
