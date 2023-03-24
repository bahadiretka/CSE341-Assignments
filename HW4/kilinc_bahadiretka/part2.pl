flight(rize, ankara, 5).
flight(van, ankara, 4).
flight(antalya, izmir, 1).
flight(ankara, diyarbakir, 8).
flight(van, gaziantep, 3).
flight(izmir, rize, 4).
flight(izmir, ankara, 6).
flight(izmir, istanbul, 3).
flight(antalya, izmir, 2).
flight(antalya, diyarbakir, 4).
flight(erzincan, antalya, 3).
flight(canakkale, erzincan, 6).

route(X, Y, C) :-
    flight(X, Y, C).

route(X, Y, T) :-
    flight(X, Z, Dist),route(Z, Y, Dist1),T is Dist1 + Dist.