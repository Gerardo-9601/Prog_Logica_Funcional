-- Distancia entre dos puntos --
distanciaPuntos (p1,q1) (p2,q2) = sqrt((p1-p2)^2+(q1-q2)^2)
--  Permutación cíclica de una lista --
cicloLista [] = []
cicloLista l = last l : init l
-- fórmula de Herón --
heron a b c = sqrt (s*(s-a)*(s-b)*(s-c))
	where s = (a+b+c)/2
