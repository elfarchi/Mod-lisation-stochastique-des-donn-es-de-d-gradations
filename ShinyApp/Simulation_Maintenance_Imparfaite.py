import numpy as np
import matplotlib.pyplot as plt

# =====================
# Paramètres du modèle
# =====================
L = 10.0        # seuil de défaillance
h = 5.0         # seuils de maintenance
a = 0.4         # facteur de réduction (0 < a < 1)

mu = 1.5        # dérive
sigma = 10       # volatilité

dt = 0.01      # pas de discrétisation
T_inspect = 0.1 # période d'inspection
Nmax = 5_000  # sécurité numérique

# =====================
# Initialisation
# =====================
X = [0]
t = [0]

X_M = []   # états juste avant maintenance
t_M = []   # instants de maintenance

next_inspection = T_inspect
n = 0

# =====================
# Simulation
# =====================
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

# =====================
# Visualisation
# =====================
plt.figure(figsize=(10, 5))
plt.plot(t, X, label="Processus X(t)")
plt.axhline(h, linestyle="--", label="Seuil maintenance h")
plt.axhline(L, linestyle="--", label="Seuil défaillance L")

if t_M:
    plt.scatter(t_M, X_M, color="red", s=20, label="Maintenances")

plt.xlabel("Temps")
plt.ylabel("État")
plt.legend()
plt.grid(True)
plt.show()
