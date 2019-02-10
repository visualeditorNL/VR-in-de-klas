import random
random.seed(34)

klas = ["E6-3hva","E6-3hvb","E6-3hvc","T3-3hva","T3-3hvb","T3-3hvc","T3-3hvd"]
onderwerp = ["geomorfologie","vegetatiezones","geomorfologie","vegetatiezones","geomorfologie","vegetatiezones",]


for i in range(5):
    index1 = random.randrange(len(klas))
    index2 = random.randrange(len(onderwerp))
    onderwerpKeuze = onderwerp[index2]
    del onderwerp[index2] 
    klasKeuze = klas[index1]
    del klas[index1]
    print(onderwerpKeuze+": "+klasKeuze)
    

print("De klassen die niet gekozen zijn: "+ str(klas))