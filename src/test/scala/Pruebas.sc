import Comete._
import Opinion._

val pi_max = Vector(0.5 , 0.0, 0.0, 0.0, 0.5)
val pi_min = Vector(0.0 , 0.0, 1.0, 0.0, 0.0)
val pi_der = Vector(0.4 , 0.0, 0.0, 0.0, 0.6)
val pi_izq = Vector(0.6 , 0.0, 0.0, 0.0, 0.4)
val pi_int1 = Vector(0.0 , 0.5, 0.0, 0.5, 0.0)
val pi_int2 = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3 = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_cons_centro = pi_min
val pi_cons_der = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_cons_izq = Vector(1.0, 0.0, 0.0, 0.0, 0.0)
val likert5 = Vector(0.0 , 0.25, 0.5, 0.75, 1.0)
val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief (100)
val sb_midly = midlyBelief(100)

val cmt1 = rhoCMT_Gen(1.2, 1.2)

cmt1(pi_max, likert5)
cmt1(pi_min, likert5)
cmt1(pi_der, likert5)
cmt1(pi_izq, likert5)
cmt1(pi_int1, likert5)
cmt1(pi_int2, likert5)
cmt1(pi_int3, likert5)
cmt1(pi_cons_centro, likert5)
cmt1(pi_cons_der, likert5)
cmt1(pi_cons_izq, likert5)

val cmt1_norm = normalizar(cmt1)
cmt1_norm(pi_max, likert5)
cmt1_norm(pi_min, likert5)
cmt1_norm(pi_der, likert5)
cmt1_norm(pi_izq, likert5)
cmt1_norm(pi_int1, likert5)
cmt1_norm(pi_int2, likert5)
cmt1_norm(pi_int3, likert5)
cmt1_norm(pi_cons_centro, likert5)
cmt1_norm(pi_cons_der, likert5)
cmt1_norm(pi_cons_izq, likert5)

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)

rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)

rho1(sb_unif, dist1)
rho2(sb_unif, dist1)
rho1(sb_unif, dist2)
rho2(sb_unif, dist2)

rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)

rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)

val rho1 = rhoPar(1.2, 1.2)
val rho2 = rhoPar(2.0, 1.0)

val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)

rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)

rho1(sb_unif, dist1)
rho2(sb_unif, dist1)
rho1(sb_unif, dist2)
rho2(sb_unif, dist2)

rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)

rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)