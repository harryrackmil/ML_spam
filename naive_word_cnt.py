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
		allwords.add(w)
		if w in wordsbytext[-1]:
			wordsbytext[-1][w] += 1/textlen
		else:
			wordsbytext[-1][w] = 1/textlen
			

allwords = list(allwords)
countmat = []
for text in wordsbytext:
	countmat.append([])
	for word in allwords:
		if word not in text:
			countmat[-1].append(0)
		else:
			countmat[-1].append(text[word])

raw_csv = open(r'C:\Users\Harry\Documents\Fall 14\Stat 154\raw_freq.csv', 'w')


raw_csv.write(str(allwords[1:-1]))
for line in countmat:
	raw_csv.write(str(line)[1:-1])

raw_csv.close()

raw_csv = open(r'C:\Users\Harry\Documents\Fall 14\Stat 154\raw_freq.csv', 'r')
