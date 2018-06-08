let reasonReactList = list => ReasonReact.array(Array.of_list(list));

let eventTargetValue = event => ReactDOMRe.domElementToObj(
                                  ReactEventRe.Form.target(event),
                                )##value;