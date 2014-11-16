#read stop words into list
stops = open(r'common-english-words.txt', 'r')
stopwords = stops.read().split(",")
stops.close()

#create list of dictionaries with 0-1 freq of each word in text
text_train = open("train_msgs.txt", encoding='utf-8')
label = []
allwords = set()
wordsbytext = []
for line in text_train:
	tabsplit = line.split("\t")
	words = [w for w in tabsplit[1][:-1].split() if w not in stopwords]
	textlen = len(words)
	if textlen > 0:
		label.append(tabsplit[0])
		wordsbytext.append({});
		for w in words:
			allwords.add(w)
			if w in wordsbytext[-1]:
				wordsbytext[-1][w] += 1/textlen
			else:
				wordsbytext[-1][w] = 1/textlen

#create freq data matrix with unique words as columns, texts as rows
allwords = list(allwords)
countmat = []
for text in wordsbytext:
	countmat.append([])
	for word in allwords:
		if word in text:
			countmat[-1].append(text[word])
		else:
			countmat[-1].append(0)
	countmat[-1].append(label[0]);
	label = label[1:]

#write to csv
raw_out = open(r'raw_mtx.csv', 'w')
raw_out.write(str(allwords)[1:-1] + ", 'label'"+ "\n")
for row in countmat:
	raw_out.write(str(row)[1:-1] + "\n")

raw_out.close()