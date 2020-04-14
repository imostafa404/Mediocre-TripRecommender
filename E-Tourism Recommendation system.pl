% offerMean(X, Y) -> Transportation mean Y is used with offer X

offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12, period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1), bus).

% offerAccommodation(X, Y) -> Accommodation Y is part of offer X

offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).

% customerPreferredActivity(X, Y, R) -> Y is the preferred activity kind wrt customer X with relevance R

customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding, 50).

% customerPreferredMean(X, Y, R) -> Y is the preferred transportaion mean wrt customer X with relevance R

customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).

% customerPreferredAccommodation(X, Y, R) -> Y is the preferred accommodation to customer X with relevance R

customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).

%1
subset1([],[]).
subset1([H|T1],[H|T2]):-
  subset1(T1,T2).
subset1([_|T1],L2):-
  subset1(T1,L2).

possibleSubset(L1,L2):-
   subset1(L1,L3),
   permutation(L3,L2).

%2
subset1([activity(L)|T1],[activity(M)|T2]):-
  subset1(L,M),
  subset1(T1,T2).
choosePreferences(L1,R1):-
  subset1(L1,R1).

%3
preferenceActivity(_,_,[],_,T,T).
preferenceActivity(_,C,[H|T],_,S,Acc):-
  customerPreferredActivity(C,H,R1),
  Acc1 is Acc + R1,
  preferenceActivity(_,C,T,_,S,Acc1).

preferenceSatisfaction2(offer(_,M,_,_,_,_,_,_),C,activity(L),_,Acc):-
  possibleSubset(M,L),
  preferenceActivity(_,C,L,_,Acc,0).
preferenceSatisfaction2(offer(_,M,_,_,_,_,_,_),_,activity(L),_,0):-
  \+possibleSubset(M,L).
 preferenceSatisfaction2(O,C,mean(H),_,Acc):-
  offerMean(O,H),
  customerPreferredMean(C,H,Acc).
preferenceSatisfaction2(O,_,mean(H),_,0):-
  \+offerMean(O,H).
preferenceSatisfaction2(O,C,accommodation(H),_,Acc):-
  offerAccommodation(O,H),
  customerPreferredAccommodation(C,H,Acc).
preferenceSatisfaction2(O,_,accommodation(H),_,0):-
  \+offerAccommodation(O,H).
preferenceSatisfaction2(O,_,A,_,Acc):-
  A \= activity(_),
  A \= mean(_),
  A \= accomodation(_),
  Acc is 0.

preferenceSatisfaction1(_,_,[],S,S).
preferenceSatisfaction1(O,C,[H|T],S,Acc):-
  preferenceSatisfaction2(O,C,H,S,R1),
  Acc1 is Acc + R1,
  preferenceSatisfaction1(O,C,T,S,Acc1).

preferenceSatisfaction(_,_,[],0).
preferenceSatisfaction(O,C,Ch,S):-
  preferenceSatisfaction1(O,C,Ch,S,0).

%4
overlapYear(period(Y1S-_-_,Y1E-_-_),period(Y2S-_-_,Y2E-_-_)):-
  (Y1E > Y2S,
  Y2E > Y2S);
  (Y2E > Y1S,
   Y1E > Y2S).
overlapMonth(period(_-M1S-_,_-M1E-_),period(_-M2S-_,_-M2E-_)):-
  (M1E > M2S,
   M2E > M2S);
  (M2E > M1S,
   M1E > M2S).
overlapDay(period(_-_-D1S,_-_-D1E),period(_-_-D2S,_-_-D2E)):-
  (D1E >= D2S,
  D2E >= D2S);
  (D2E >= D1S,
   D1E >= D2S).
overlapYear(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)):-
  (Y1E = Y2S;
  Y2E = Y1S),
  overlapMonth(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)).
overlapMonth(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)):-
  (M1E = M2S;
  M2E = M1S),
  overlapDay(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)).
overlapPeriod1(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)):-
  overlapYear(period(Y1S-M1S-D1S,Y1E-M1E-D1E),period(Y2S-M2S-D2S,Y2E-M2E-D2E)).


overlapPeriod(P1,P2):-
  overlapPeriod1(P1,P2).

%5
getOffer3(dest(X),offer(X,_,_,_,_,_,_,_)).
getOffer3(period(X,W),offer(_,_,_,_,_,period(Y,Z),_,_)):-
  overlapPeriod(period(Y,Z),period(X,W)).
getOffer3(budget(X),offer(_,_,Y,_,_,_,_,_)):-
  Y =< X.
getOffer3(activity(X),offer(_,Y,_,_,_,_,_,_)):-
  possibleSubset(Y,X).
getOffer3(mean(X),O):-
  offerMean(O,X).
getOffer3(accommodation(X),O):-
  offerAccommodation(O,X).
getOffer2([],_).
getOffer2([H|T],O):-
  getOffer3(H,O),
  getOffer2(T,O).
getOffer1(ChosenPrefs,O):-
  offerMean(O,_),
  getOffer2(ChosenPrefs,O).
getOffer(ChosenPrefs,Offer):-
  getOffer1(ChosenPrefs,Offer).

%6
recommendOfferForCustomer(Prefs,ChosenPrefs, O):-
  subset1(Prefs,ChosenPrefs),
  getOffer(ChosenPrefs,O).

%7

customerSatisfactionGenerator([], [], _, ACC, ACC).
customerSatisfactionGenerator([H1|T1], [H2|T2], Offer, X, ACC):-
  preferenceSatisfaction(Offer, H1, H2,S),
  append(X,[S],Y),
  customerSatisfactionGenerator(T1, T2, Offer, Y, ACC).

sorter(Prefs, List,Sortedc):-
  i_sort(Prefs, List,[], [], _, Sortedc).
i_sort([], _, Acck, Accc, Acck, Accc).
i_sort([Hk|Tk],[Hc|Tc], Acck, Accc, Sortedk, Sortedc):-
  insert(Hk,Hc,Acck, Accc, NAcck, NAccc),
  i_sort(Tk, Tc,NAcck, NAccc, Sortedk, Sortedc).
insert(Xk,Xc,[],[], [Xk],[Xc]).
insert(Xk,Xc,[Yk|Tk],[Yc|Tc],[Yk|NTk],[Yc|NTc]):-
  Xk<Yk,
  insert(Xk,Xc,Tk,Tc,NTk,NTc).
insert(Xk,Xc,[Yk|Tk],[Yc|Tc],[Xk,Yk|Tk],[Xc,Yc|Tc]):-
  Xk>=Yk.

customerLimiter1([],_,R,R).
customerLimiter1(_,0,R,R).
customerLimiter1([H|T],N,Acc,R):-
  N>0,
  append(Acc,[H],R1),
  N1 is N-1,
  customerLimiter1(T,N1,R1,R).
customerLimiter(List, offer(_,_,_,_,_,_,_,N),R):-
  customerLimiter1(List,N,[],R).

offerForEachCustomer([P|_],Offer):-
  recommendOfferForCustomer(P,_,Offer).
offerForEachCustomer([_|T],Offer):-
  offerForEachCustomer(T,Offer).

recommendOffer1(Customers, PreferenceList, Offer, CustomersChosen):-
  %offerForEachCustomer(PreferenceList,Offer),
  offerMean(Offer,_),
  customerSatisfactionGenerator(Customers, PreferenceList, Offer, [], Prefs),
  sorter(Prefs, Customers, SortedCustomers),
  customerLimiter(SortedCustomers,Offer,CustomersChosen).

recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):-
  recommendOffer1(Customers, PreferenceList, Offer, CustomersChosen).



















