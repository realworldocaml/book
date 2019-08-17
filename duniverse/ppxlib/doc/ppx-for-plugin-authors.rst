**********************
PPX for plugin authors
**********************

This section describes how to use ``ppxlib`` for PPX plugin authors.

Metaquot
--------

``metaquot`` is a PPX plugin that helps you write PPX plugins. It lets you write AST node values
using the actual corresponding OCaml syntax instead of building them with the more verbose AST types
or ``Ast_builder``.

To use ``metaquot`` you need to add it to the list of preprocessor for your PPX plugin:

.. code:: scheme

          (library
           (name my_plugin_lib)
           (preprocess (pps ppxlib.metaquot)))

``metaquot`` can be used both to write expressions of some of the AST types or to write patterns to
match over those same types. The various extensions it exposes can be used in both contexts,
expressions or patterns.

The extension you should use depends on the type of AST node you're trying to write or to
pattern-match over. You can use the following extensions with the following syntax:

- ``expr`` for ``Parsetree.expression``: ``[%expr 1 + 1]``
- ``pat`` for ``Parsetree.pattern``: ``[%pat? ("", _)]``
- ``type`` for ``Parsetree.core_type``: ``[%type: int -> string]``
- ``stri`` for ``Parsetree.structure_item``: ``[%stri let a = 1]``
- ``sigi`` for ``Parsetree.signature_item``: ``[%sigi: val i : int]``
- ``str`` and ``sig`` respectively for ``Parsetree.structure`` and ``Parsetree.signature``. They use
  similar syntax to the ``_item`` extensions above as they are just a list of such items.

If you consider the first example ``[%expr 1 + 1]``, in an expression context, ``metaquot`` will
actually expand it into:

.. code:: ocaml

          {
            pexp_desc =
              (Pexp_apply
                 ({
                    pexp_desc = (Pexp_ident { txt = (Lident "+"); loc });
                    pexp_loc = loc;
                    pexp_attributes = []
                  },
                   [(Nolabel,
                      {
                        pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                        pexp_loc = loc;
                        pexp_attributes = []
                      });
                   (Nolabel,
                     {
                       pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                       pexp_loc = loc;
                       pexp_attributes = []
                     })]));
            pexp_loc = loc;
            pexp_attributes = []
          }

For this to compile you need the AST types to be in the scope so you should always use ``metaquot``
where ``Ppxlib`` is opened.
You'll also note that the generated node expects a ``loc : Location.t`` value to be available. The
produced AST node value and every other nodes within it will be located to ``loc``. You should make
sure ``loc`` is the location you want for your generated code when using ``metaquot``.

When using the pattern extension, it will produce a pattern that matches no matter what the
location and attributes are. For the previous example for instance, it will produce the following
pattern:

.. code:: ocaml

          {
            pexp_desc =
              (Pexp_apply
                 ({
                    pexp_desc = (Pexp_ident { txt = (Lident "+"); loc = _ });
                    pexp_loc = _;
                    pexp_attributes = _
                  },
                   [(Nolabel,
                      {
                        pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                        pexp_loc = _;
                        pexp_attributes = _
                      });
                   (Nolabel,
                     {
                       pexp_desc = (Pexp_constant (Pconst_integer ("1", None)));
                       pexp_loc = _;
                       pexp_attributes = _
                     })]));
            pexp_loc = _;
            pexp_attributes = _
          }

Using these extensions alone, you can only produce constant/static AST nodes. You can't bind
variables in the generated patterns either.
``metaquot`` has a solution for that as well: anti-quotation.
You can use anti-quotation to insert any expression or pattern representing an AST node.
That way you can include dynamically generated nodes inside a ``metaquot`` expression extension point
or use a wildcard or variable pattern in a pattern extension.

Consider the following example:

.. code:: ocaml

          let with_suffix_expr ~loc s =
            let dynamic_node = Ast_builder.Default.estring ~loc s in
            [%expr [%e dynamic_node] ^ "some_fixed_suffix"]

The ``with_suffix_expr`` function will create an ``expression`` which is the concatenation of the
``s`` argument and the fixed suffix. I.e. ``with_suffix_expr "some_dynamic_stem"`` is equivalent to
``[%expr "some_dynamic_steme" ^ "some_fixed_suffix"]``.

Similarly if you want to ignore some parts of AST nodes and extract some others when
pattern-matching over them, you can use anti-quotation:

.. code:: ocaml

          match some_expr_node with
          | [%expr 1 + [%e? _] + [%e? third]] -> do_something_with third

The syntax for anti-quotation depends on the type of the node you wish to insert:

- ``e`` to anti-quote values of type ``Parsetree.expression``: ``[%expr 1 + [%e some_expr_node]]``
- ``p`` to anti-quote values of type ``Parsetree.pattern``:
  ``[%pat? (1, [%p some_pat_node]]``
- ``t`` to anti-quote values of type ``Parsetree.core_type``:
  ``[%type: int -> [%t some_core_type_node]]``
- ``m`` to anti-quote values of type ``Parsetree.module_expr`` or ``module_type``:
  ``[%expr let module M = [%m some_module_expr_node]]`` or
  ``[%sigi: module M : [%m some_module_type_node]]``
- ``i`` to anti-quote values of type ``Parsetree.structure_item`` or ``signature_item``:
  ``[%str let a = 1 [%%i some_structure_item_node]]`` or
  ``[%sig: val a : int [%%i some_signature_item_node]]``

Note that when anti-quoting in a pattern context you must always use the ``?`` in the anti-quotation
extension as its payload should always be a pattern the same way it must always be an expression
in an expression context.

As you may have noticed, you can anti-quote expressions which type differs from the type of the
whole ``metaquot`` extension point. E.g. you can write:

.. code:: ocaml

          let structure_item =
            [%stri let [%p some_pat] : [%t some_type] = [%e some_expr]]

