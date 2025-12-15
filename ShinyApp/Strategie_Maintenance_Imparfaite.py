import numpy as np
import matplotlib.pyplot as plt
import time

t1 = time.time()

# =====================
# Paramètres du modèle
# =====================
L = 10.0        # seuil de défaillance
h_values = np.arange(6.5, 7.5, 0.1)        # seuils de maintenance
J_values = []   # Couts correspondants
a = 0.6        # facteur de réduction (0 < a < 1)
Cm = 20
Cf = 100

mu = 1        # dérive
sigma = 10       # volatilité

dt = 0.01      # pas de discrétisation
T_inspect = 0.1 # période d'inspection
Nmax = 100_000  # sécurité numérique

# =====================
# Fonction Cout
# =====================
def cout(Cm, Cf, esp_Nm, esp_Tf) :
    return (Cm * esp_Nm + Cf)/esp_Tf

def mean(liste) :
    return sum(liste)/len(liste)

# =====================
# Simulation
# =====================
def J(h) :
    Tf_values = []
    Nm_values = []
    b_values = []
    for _ in range(1000) :
        Nm, Tf, b = simulate(h)
        Tf_values.append(Tf)
        Nm_values.append(Nm)
        b_values.append(b)

    esp_Tf = mean(Tf_values)
    esp_Nm = mean(Nm_values)
    esp_b = mean(b_values)
    J_h = cout(Cm, Cf, esp_Nm, esp_Tf)
    return J_h, esp_b

def simulate(h) :
    X = [0]
    t = [0]

    X_M = []   # états juste avant maintenance
    t_M = []   # instants de maintenance

    next_inspection = T_inspect
    n = 0

    while X[-1] < L and n < Nmax:
        # incrément brownien
        dB = np.sqrt(dt) * np.random.normal()

        # évolution Wiener
        X_new = X[-1] + mu * dt + sigma * dB
        t_new = t[-1] + dt

        # défaillance : arrêt immédiat
        if X_new >= L:
            X.append(X_new)
            t.append(t_new)
            break

        # inspection périodique
        if t_new >= next_inspection:
            if X_new >= h:
                t_M.append(t_new)
                X_M.append(X_new)
                X_new = a * X_new   # maintenance imparfaite (impulsion)
            next_inspection += T_inspect

        X.append(X_new)
        t.append(t_new)
        n += 1

    Tf = t[-1]
    Nm = len(t_M)
    b = 1 # Simulation terminee avant Nmax
    if n >= Nmax :
        b = 0 # Simulation n'est pas terminee

    return Nm, Tf, b


min_idx = 0
for i in range(len(h_values)) : 
    J_h, taux_completion = J(h_values[i])
    J_values.append([round(J_h, 3), taux_completion])
    if J_values[min_idx][0] > J_values[i][0] :
        min_idx = i


t2 = time.time()
print((t2 - t1)/60)

print(J_values)
print(h_values[min_idx])

# # =====================
# # Visualisation
# # =====================
# plt.figure(figsize=(10, 5))
# plt.plot(t, X, label="Processus X(t)")
# plt.axhline(h, linestyle="--", label="Seuil maintenance h")
# plt.axhline(L, linestyle="--", label="Seuil défaillance L")

# if t_M:
#     plt.scatter(t_M, X_M, color="red", s=20, label="Maintenances")

# plt.xlabel("Temps")
# plt.ylabel("État")
# plt.legend()
# plt.grid(True)
# plt.show()
