determiner --> [the] | [a] | [an].

noun --> [cat] | [dog] | [bird] | [fish]
       | [teacher] | [student] | [friend]
       | [apple] | [car] | [house] | [computer].


personal_pronoun --> [i] | [you] | [he] | [she] | [it] | [we] | [they].


adjective --> [big] | [small] | [angry] | [beautiful].
adjectives --> [] | adjective, adjectives.

adverb --> [quickly] | [slowly] | [happily] | [sadly] | [silently] | [loudly].

verb_present --> [eat] | [eats] | [see] | [sees]
               | [run] | [runs] | [drive] | [drives]
               | [study] | [studies] | [play] | [plays].

verb_past --> [ate] | [saw] | [ran] | [drove] | [studied] | [played].

base_verb --> [eat] | [see] | [run] | [drive] | [study] | [play].

verb_future --> [will], base_verb.

verb_any --> verb_present.
verb_any --> verb_past.
verb_any --> verb_future.

sentence --> noun_phrase, verb_phrase.

noun_phrase --> personal_pronoun.
noun_phrase --> determiner, adjectives, noun.

verb_phrase --> verb_any, noun_phrase.
verb_phrase --> adverb, verb_any, noun_phrase.
verb_phrase --> verb_any, adverb, noun_phrase.


% Parsing (recognition) examples:
% ?- phrase(sentence, [the, big, dog, eats, the, fish]).
% true.
%
% ?- phrase(sentence, [i, will, eat, an, apple]).
% true.
%
% ?- phrase(sentence, [she, quickly, studies, the, computer]).
% true.
