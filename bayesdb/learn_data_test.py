from bayesdb.client import Client
client = Client()
# infer data into an object
query = client('INFER real_symptom_id, symptom_id FROM dialysisai WITH CONFIDENCE 0.9;', {})
# get the actual data
data = query[0]['data']
# init stat data
tn, tp, fn, fp, pos, neg = 0, 0, 0, 0, 0, 0
for tuple in data:
  # ignore non-inferred values
  if (tuple[1] != tuple[2]):
    original = int(tuple[1])
    inferred = int(tuple[2][1])
    # for negatives
    if (original == 1):
      neg+=1
      # tn
      if (inferred == 1): tn+=1
      # fp
      if (inferred == 8): fp+=1
    # for positives
    elif (original == 8):
      pos+=1
      # tp
      if (inferred == 8): tp+=1
      # fn
      if (inferred == 1): fn+=1

# basic stats
tprate = tp/float(pos)
fprate = fp/float(neg)
precision = tp/float(tp+fp)
recall = tp/float(tp+fn)
fmeasure = 2*tp/float(2*tp + fp + fn)

info = {
  'title': ' BayesDB Classification '.center(30, '=') + '\n\n',
  'tp': 'TP:'.ljust(20, ' ') + '%d'%tp + '\n',
  'tn': 'TN:'.ljust(20, ' ') + '%d'%tn + '\n',
  'fp': 'FP:'.ljust(20, ' ') + '%d'%fp + '\n',
  'fn': 'FN:'.ljust(20, ' ') + '%d'%fn + '\n',
  'tprate': 'TP Rate:'.ljust(20, ' ') + '%f'%tprate + '\n',
  'fprate': 'FP Rate:'.ljust(20, ' ') + '%f'%fprate + '\n',
  'precision': 'Precision:'.ljust(20, ' ') + '%f'%precision + '\n',
  'recall': 'Recall:'.ljust(20, ' ') + '%f'%recall + '\n',
  'fmeasure': 'F-Measure:'.ljust(20, ' ') + '%f'%fmeasure + '\n'
}

print info['title'],
print info['tp'],
print info['tn'],
print info['fp'],
print info['fn'],
print info['tprate'],
print info['fprate'],
print info['precision'],
print info['recall'],
print info['fmeasure']

f = open('learn_data_test.log', 'w')
f.write(info['title'])
f.write(info['tp'])
f.write(info['tn'])
f.write(info['fp'])
f.write(info['fn'])
f.write(info['tprate'])
f.write(info['fprate'])
f.write(info['precision'])
f.write(info['recall'])
f.write(info['fmeasure'])
f.close()