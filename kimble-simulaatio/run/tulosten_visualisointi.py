import numpy as np
import matplotlib.pyplot as plt



asetukset = np.loadtxt("asetukset.dat")
pelaaja_1 = np.loadtxt("Pelitaktiikka_1_tulokset.txt")
pelaaja_2 = np.loadtxt("Pelitaktiikka_2_tulokset.txt")
pelaaja_3 = np.loadtxt("Pelitaktiikka_3_tulokset.txt")
pelaaja_4 = np.loadtxt("Pelitaktiikka_4_tulokset.txt")

tulokset = [pelaaja_1 , pelaaja_2, pelaaja_3, pelaaja_4]

for i in range(3):
    asetukset[i] = int(asetukset[i])

# Laitetaan nää prosenteiks ni helpompi tuijotella
for i in range(len(tulokset)):
    tulokset[i] = 100.0/asetukset[0]*tulokset[i]





vali = [1, 2, 3, 4]

fig, ax = plt.subplots(2,2)


plt.suptitle('Pelaajien sijoitusjakaumat')

fig.tight_layout(pad=3.0)

ax[0,0].set_xticks(vali)
ax[0,0].bar(vali, tulokset[0], width = 0.5)
ax[0,0].set_title("Pelaajan 1 sijoitusjakauma")

ax[1,0].set_xticks(vali)
ax[1,0].bar(vali, tulokset[1], width = 0.5)
ax[1,0].set_title("Pelaajan 2 sijoitusjakauma")
ax[1,0].set_xlabel("Sijoitus")

ax[0,1].set_xticks(vali)
ax[0,1].bar(vali, tulokset[2], width = 0.5)
ax[0,1].set_title("Pelaajan 3 sijoitusjakauma")


ax[1,1].set_xticks(vali)
ax[1,1].bar(vali, tulokset[3], width = 0.5)
ax[1,1].set_title("Pelaajan 4 sijoitusjakauma")
ax[1,1].set_xlabel("Sijoitus")

plt.savefig("Histogrammit")
