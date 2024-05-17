parent(abraham, isaac).parent(isaac, jacob).
parent(jacob, juda).parent(jacob, ruben). parent(jacob, simeon).
parent(jacob, levi).
parent(jacob, isacar).
parent(jacob, zabulon).
parent(jacob, dina).
parent(jacob, dan).
parent(jacob, neftali).
parent(jacob, gad).
parent(jacob, aser).
parent(jacob, jose).
parent(jacob, benjamin).
parent(juda, fares).
parent(juda, zera).
parent(tamar, fares).
parent(tamar, zera).
parent(fares, jezron).
parent(jezron, aram).
parent(aram, aminadab).
parent(aminadab, naason).
parent(naason, salmon).
parent(salmon, booz).
parent(rajab, booz).
parent(booz, obed).
parent(obed, isai).
parent(isai, david).
parent(david, salomon).
parent(salomon, roboan).
parent(roboan, abias).
parent(abias, asa).
parent(asa, josafat).
parent(josafat, joran).
parent(joran, uzias).
parent(uzias, jotan).
parent(jotan, acaz).
parent(acaz, ezequias).
parent(ezequias, manases).
parent(manases, amon).
parent(amon, josias).
parent(josias, jeconias).
parent(jeconias, salatiel).
parent(salatiel, zorobabel).
parent(zorobabel, abiud). parent(abiud, eliaquin).
parent(eliaquin, azor). parent(azor, sadoc).
parent(sadoc, aquin). parent(aquin, eliud).
parent(eliud, eleazar). parent(eleazar, matan).
parent(matan, jacob).parent(jacob, jose).
parent(jose, jesus).parent(maria, jesus).
male(jose).
abuelos(X,Y):-parent(X,Z),parent(Z,Y).,male(X).
