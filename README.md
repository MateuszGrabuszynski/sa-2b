> **Note:** By no means is this code secure or validated. Treat as unchecked copy archived due to no changes planned.

# Trading FSM
Following code is based on the work of Frederic Trottier-Hebert, author of _Learn You Some Erlang for great good!_. The original MIT license is available in `LICENSE-original.txt`. Finite-State Machines are described there: https://learnyousomeerlang.com/finite-state-machines. More about trading machine is in _A Trading System Specification_ section (#a-trading-system-specification).

I am aware that this solution is **not fully solving the exercise**, as the prices are not checked while biding. This code may, or may not be upgraded in the future. Also, some of the functions used are already obsolete. I chose MIT license as appropriate and it's full text is available in `LICENSE.txt`.

## Exercise
The task was wrote in Polish by dr inż. Grażyna Brzykcy as homework assignment for IT students on Poznań University of Technology. The subject "Systemy agentowe" would translate to "Agent systems".

### Original (Polish)
```
Zmodyfikuj przykład „Trading System” w taki sposób, by negocjacje doty-
czyły ceny jednego produktu (gracze to producent i konsument, a kolejne
oferty mają polegać na zmniejszaniu ceny proponowanej przez producenta
i zwiększaniu tej proponowanej przez konsumenta).
```
### Translated
```
Modify example "Trading System" so that the negotiations pertain
to price of one product (players are producer and consumer, next
offers consist of lowering the price proposed by the producer
and raising the one bided by the consumer).
```

## Running
1. Open Erlang (preferably GUI, werl.exe),
2. "cd" to the catalog where the files are, eg. `cd("d:/Erlang/is/awesome/").`,
3. Compile the necessary files `c("trade_fsm"). c("trade_run")`; do not mind the warnings; this will create beam files,
4. Run main_ab() `trade_run:main_ab().`.
