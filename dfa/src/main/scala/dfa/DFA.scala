package dfa // leave this line in the file

case class State(label: String)

case class Transition(from: State, to: State, symbol: Char)

class DFA(
    val states: Set[State],
    val transitions: Set[Transition],
    val start: State,
    val accept: Set[State]):

    def accepts(input: String): Boolean = 
        // First we set the current state to the start
        var currentState: State = start
        for (currentSymbol <- input)
            // Get transition where from == current state and symbol == current symbol
            // Then call head to get extract value from set and set the current state to the to State
            currentState = (transitions.filter(t =>(t.from == currentState && t.symbol == currentSymbol)).head.to)

        // Finally we retun if the currentState is an accepting state
        accept.contains(currentState)

