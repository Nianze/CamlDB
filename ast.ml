open Table

type name = string

type typ = Table.t

type operator = Table.operator

type cond = Table.condition

type c_tree = Table.cond_tree

type top_t = Table.top_t

type order = Table.order

type plot = Visualizer.vis_method

type expr =
  | Int      of int
  | Bool     of bool
  | String   of string
  | TbName   of name
  | ColName  of name
  | Path     of expr * expr
  | SelCol   of expr list * expr * plot
  | SelTop   of top_t * expr list * expr * plot
  | Distin   of expr * expr * plot
  | Where    of c_tree * expr
  | Sort     of expr * order * expr
  | InsRow   of typ list * expr
  | InsCol   of expr list * typ list * expr
  | UpdAll   of (expr * typ) list * expr
  | Update   of c_tree * (expr * typ) list * expr
  | DelAll   of expr
  | Delete   of c_tree * expr
  | Create   of expr * (expr * typ) list
  | Union    of expr * expr
  | Joins    of expr * expr * expr list * (expr * expr)
  | Err
