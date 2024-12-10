# Projet Traduction des Langages

Projet réalisé à l'N7 pour la matière Traduction des Langages.

## Ajout des pointeurs

### Nouveaux tokens

Les pointeurs introduisent de nouveaux tokens :

- `*` : Pour définir un pointeur
- `&` : Pour accéder à la valeur d'un pointeur
- `new` : Pour allouer de la mémoire pour une nouvelle variable (Ex : `int *x = (new int)`)
- `null` : Pointeur nul

### Nouvelles règles de grammaire

- `TYPE -> TYPE*`
- `E -> A`
- `E -> &id`
- `E -> null`
- `E -> (new TYPE)`
- `A -> *A`
- `A -> id`
- `I -> A = E`

### AstSyntax

Les pointeurs introduisent un nouveau type :

```ocaml
type typ = Bool|Int|Rat|Undefined|Pointeur of typ

type affectable = 
    | Ident of string
    | Deref of affectable

type instruction = 
    | Declaration of typ * string * exp
    | Affectation of affectable * exp

type expression = 
(*| Ident of String*)
| Affectable  of affectable
| Adresse of string
| New of typ
| Null
```

### AstTds

```ocaml
type affectable = 
    | Ident of info_ast
    | Deref of affectable

type instruction = 
    | Declaration of typ * info_ast * exp
    | Affectation of affectable * exp

type expression = 
(*| Ident of String*)
| Affectable  of affectablev
| Adresse of info_ast
| New of typ
| Null
```

L'analyse est plus délicate car maintenant on peut avoir des expressions à GAUCHE et AUSSI à DROITE d'un égale. Ainsi, voici l'analyse :

```ocaml
    (*Introduction d'un booleen modif pour savoir sui on est à gauche ou à droite d'un égal*)
    analyser_tds_affectable tds a modif = 
        |Ident n -> ... if modif then ...
        |Deref v -> ... analyser_tds_affectable ...

    analyser_tds_instruction tds i ioa= 
        |Affectation(a,e) ->
            analyser_tds_affectable tds a true ...

    analyse_tds_expr tds e = 
        |Affectable(a) -> analyser_tds_affectable tds e false ...
```

### AstType

On met comme avant les types dans les infos. Ainsi, seulement instruction change :

```ocaml
type instruction = 
    | Declaration of (*typ * *) info_ast * exp
    | Affectation of affectable * exp
```

Pour l'analyse :

Régles :

$$
\begin{array}{c}
\sigma \vdash a : Pointeur(\tau) \\
\hline
\sigma \vdash *a : \tau
\end{array}
$$

$$
\begin{array}{c}
\sigma \vdash a : \tau \\
\hline
\sigma \vdash \&a : Pointeur(\tau)
\end{array}
$$

$$
\begin{array}{c}
\sigma \vdash T : \tau \\
\hline
\sigma \vdash (new ~ T) : Pointeur(\tau)
\end{array}
$$

$$
\begin{array}{c}
\sigma \vdash t : \tau \\
\hline
\sigma \vdash t* : Pointeur(\tau)
\end{array}
$$

```ocaml
analyser_type_affectable a = match a with
    |Ident(info) -> 
        begin
            match info with
                |InfoVar _,t,_,_ -> (Ident info, t)
                |InfoConst _,v,_,_ -> (Ident info, Int)
                | _ -> failwith "Erreur"
        end
    |Deref(aff) -> 
        begin
            match analyse_type_affectatable aff in
                |(naff, Pointeur(t)) -> (Dref naff, t)
                | _ -> failwith "Erreur"
        end
```

### AstPlacement

On doit rajouter le `getTaille(Pointeur) = 1` et BOOM c'est fini

### La merde commence (Le code TAM)

#### Exemple de convertion en code TAM

Le code RAT
```ocaml
pointeur {
    int * x = (new int);
    * x = 4;
    int z = 18;
    int *y = & z;
    *y = *x;
}
```

Le code TAM
```C
PUSH 1
LOADL 1 // car x est un pointeur sur un entier (taille 1)
SUBR Malloc
STORE (1) 0[SB]

LOADL 4
LOAD (1) 0[SB]
STOREI (1)

PUSH 1
LOADL 18
STORE (1) 1[SB]

PUSH 1
LOADA 1[SB]
STORE (1) 2[SB]

LOAD (1) 0[SB]
LOADI (1)
LOAD (1) 2[SB]
STOREI (1)

POP (0) 3
```
