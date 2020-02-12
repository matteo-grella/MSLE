# MLSE (Markov Logic Sentence Encoder)

This is an experimental implementation of the Poon and Domingos system (2009) written in pure Ada. The original Java algorithm has been completely revised.
Our implementation starts directly with dependency trees encoded in the well-knwon CoNLL format and allows to distribute the computation.

> The system transforms dependency trees into quasi-logical forms, recursively induces lambda forms from these, and clusters them to abstract away syntactic variations of the same meaning. The MAP semantic parse of a sentence is obtained by recursively assigning its parts to lambda-form clusters and composing them.

## Basic usage

```sh
cd bin
./parse_master parse_master_config.xml ../data/genia/ .
./eval ../data/eval/ . ../data/genia/
```

## References
Hoifung Poon and Pedro Domingos: Unsupervised Semantic Parsing. EMNLP (2009)

## Credits
Matteo Grella, Marco Nicola (2012)

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.