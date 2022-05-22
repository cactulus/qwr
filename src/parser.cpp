#include <cassert>
#include <iostream>
#include <sstream>
#include <cstring>
#include <algorithm>

#include "parser.h"
#include "builtin.h"
#include "manager.h"

static void init_preproc_definitions();

const std::unordered_map<int, int> operator_precedence = {
	{TOKEN_ADD_EQ, 1},
	{TOKEN_SUB_EQ, 1},
	{TOKEN_MUL_EQ, 1},
	{TOKEN_DIV_EQ, 1},
	{TOKEN_MOD_EQ, 1},
	{'=', 1},
    {TOKEN_BAR_BAR, 2},
    {TOKEN_AND_AND, 3},
    {'|', 4},
    {'^', 5},
    {'&', 6},
    {TOKEN_EQ_EQ, 7},
    {TOKEN_NOT_EQ,7},
    {TOKEN_LT_EQ, 8},
    {TOKEN_GT_EQ, 8},
    {'<', 8},
    {'>', 8},
    {TOKEN_SHL, 9},
    {TOKEN_SHR, 9},
	{'+', 10},
	{'-', 10},
	{'*', 11},
	{'/', 11},
	{'%', 11},
};

static std::vector<std::string> preproc_definitions = {};

static FunctionDefinition *current_function;

void Parser::init(Typer *_typer, Messenger *_messenger) {
	typer = _typer;
	messenger = _messenger;

	scope = new Scope(messenger);
	has_reached_end = false;
	
    init_preproc_definitions();
}

Stmt *Parser::parse_top_level_stmt() {
	auto tok = lexer.peek_token();
        
    if (tok->type == TOKEN_ATOM) {
        auto pt = lexer.peek_token(1)->type;
        if (pt == ':') {
            lexer.eat_token();
            lexer.eat_token();

            if (eat_token_if('=')) {
                return parse_variable_definition(tok, VAR_GLOBAL);
            } else if (eat_token_if('@')) {
                return parse_variable_definition(tok, VAR_GLOBAL | VAR_CONST);
            } else {
                return parse_variable_definition_type(tok, VAR_GLOBAL);
            }
        } else if (pt == TOKEN_COLON_COLON) {
            lexer.eat_token();
            lexer.eat_token();

            if (eat_token_if(TOKEN_ENUM)) {
                parse_enum(tok);
                return 0;
            } else if (eat_token_if(TOKEN_STRUCT)) {
                parse_struct(tok);
                return 0;
            } else {
                return parse_func_def(tok, 0x0);
            }
        } else {
            messenger->report(lexer.peek_token(2), "Unexpected token");
        }
    }

    if (eat_token_if(TOKEN_EXTERN) || eat_token_if(TOKEN_BUILTIN)) {
		auto flag_token = tok;
		tok = lexer.peek_token();
        if (tok->type == TOKEN_ATOM) {
            auto pt = lexer.peek_token(1)->type;
            if (pt == TOKEN_COLON_COLON) {
                lexer.eat_token();
                lexer.eat_token();

				if (flag_token->type == TOKEN_EXTERN) {
					return parse_extern_func_def(tok);
				} else {
					return parse_func_def(tok, FUNCTION_BUILTIN);
				}
            } else {
                messenger->report(lexer.peek_token(2), "Unexpected token");
            }
        } else {
            messenger->report(tok, "Expected function name after 'extern'");
        }
    }

    if (eat_token_if(TOKEN_USE)) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_STRING_LIT)) {
            messenger->report(tok, "Expected string literal with name of standard library");
        }
        eat_token_if(';');
    
        manager_add_library(tok->lexeme);
        return 0;
    }

    if (eat_token_if(TOKEN_INCLUDE)) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_STRING_LIT)) {
            messenger->report(tok, "Expected string literal with name of file");
        }
        eat_token_if(';');
    
        manager_add_src_file(tok->lexeme);
        return 0;
    }

	if (eat_token_if(TOKEN_TYPEDEF)) {
		auto tok = lexer.peek_token();
		if (!eat_token_if(TOKEN_ATOM)) {
			messenger->report(tok, "Expected identifier for type name");
		}
		auto ty = parse_type();
		eat_token_if(';');

		typer->make_ref_type(tok, ty);

		return 0;
	}

    if (eat_token_if(TOKEN_QWR)) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_STRING_LIT)) {
            messenger->report(tok, "Expected string literal after 'qwr' for specifying linker lags");
        }
        eat_token_if(';');
    
        manager_add_flags(tok->lexeme);

        return 0;
    }

    if (eat_token_if(TOKEN_PREPROC)) {
        return parse_preproc(tok, true);
    }

    if (tok->type == TOKEN_EOF) {
        has_reached_end = true;
    } else {
        messenger->report_print_token(tok, "Unexpected token");
    }

	return 0;
}

Stmt *Parser::parse_stmt() {
	auto tok = lexer.peek_token();

	if (eat_token_if('{')) {
	    return parse_block();
    }

    if (eat_token_if(TOKEN_IF)) {
        return parse_if();
    }

    if (eat_token_if(TOKEN_WHILE)) {
        return parse_while();
    }

	if (eat_token_if(TOKEN_FOR)) {
		return parse_for();
	}

	if (eat_token_if(TOKEN_RETURN)) {
		return parse_return();
	}

	if (eat_token_if(TOKEN_DELETE)) {
		return parse_delete();
	}

    if (eat_token_if(TOKEN_PREPROC)) {
        return parse_preproc(tok, false);
    }

    if (eat_token_if(TOKEN_USING)) {
        parse_using();
        return 0;
    }

	return try_parse_atom();
}

Stmt *Parser::parse_preproc(Token *op_token, bool top_level) {
    if (strcmp(op_token->lexeme, "if") == 0) {
        auto arg = lexer.peek_token();
        if (!eat_token_if(TOKEN_ATOM)) {
            messenger->report(arg, "Expected identifier");
        }

        auto begin = preproc_definitions.begin();
        auto end = preproc_definitions.end();
        if (std::find(begin, end, arg->lexeme) == end) {
            if (eat_token_if('{')) {
                while (!eat_token_if('}'))
                    lexer.eat_token();
            } else {
                while (!eat_token_if(';'))
                    lexer.eat_token();
            }
            return 0;
        }

        if (top_level) {
            return parse_top_level_stmt();
        }
        return parse_stmt();
    }

    messenger->report(op_token, "Undefined preprocessor directive");
    return 0;
}

void Parser::parse_enum(Token *name) {
	if (!eat_token_if('{')) {
		messenger->report(lexer.peek_token(), "Expected { after enum name");
	}
    
	auto type = typer->make_type(name, TYPE_ENUM, 0);
    type->categories = new std::vector<const char *>();
    type->indices = new std::vector<unsigned int>();

    unsigned int index = 0;
    while (!eat_token_if('}')) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_ATOM)) {
            messenger->report(tok, "Only identifiers allowed in enum definition");
        }

        if (eat_token_if('=')) {
            auto int_lit = lexer.peek_token();
            if (!eat_token_if(TOKEN_INT_LIT)) {
                messenger->report(int_lit, "Expected integer literal");
            }
            
            unsigned int new_index = int_lit->int_value;
            if (new_index < index) {
                messenger->report(int_lit, "Enum values have to be ascending. Given integer is smaller than some enum value before.");
            }
            index = new_index;
        }

        type->categories->push_back(tok->lexeme);
        type->indices->push_back(index);

        eat_token_if(',');
        index++;
    }
}

void Parser::parse_struct(Token *name) {
	if (!eat_token_if('{')) {
		messenger->report(lexer.peek_token(), "Expected { after struct name");
	}
     
    auto fields = new struct_fields_type();

    while (!eat_token_if('}')) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_ATOM)) {
            messenger->report(tok, "Expected identifier");
        }

        auto fty = parse_type();

        fields->push_back(std::make_pair(tok->lexeme, fty));

        eat_token_if(',');
    }

    typer->make_struct(name->lexeme, fields);
}

void Parser::parse_using() {
    auto tok = lexer.peek_token();
         
    if (typer->has(tok->lexeme) && typer->get(tok->lexeme)->isenum()) {
        lexer.eat_token();
        
        auto enum_type = typer->get(tok->lexeme);
        auto categories = *enum_type->categories;
        auto indices = *enum_type->indices;
        for (int i = 0; i < categories.size(); ++i) {
            auto var = new Variable(typer->get("u64"), tok);
            var->flags |= VAR_PROXY_ENUM;
            var->name = categories[i];
            var->proxy_index = indices[i];

            scope->add_proxy(tok, var);
        }
    } else {
        auto var_expr = parse_expr();
        auto st_ty = var_expr->type; 

        if (var_expr->kind() != VARIABLE) {
            messenger->report(tok, "Illegal target for using");
        }
        
        bool dereference = false;
        if (st_ty->ispointer() && st_ty->element_type->isstruct()) {
            st_ty = st_ty->element_type;
            dereference = true;
        } else if (!st_ty->isstruct()) {
            messenger->report(tok, "Using only works on enum types and variables of struct type!");
        }

        int index = 0;
        for (auto field : *st_ty->fields) {
            auto member_name = field.first;
            auto member_type = field.second;
            auto member = new Member(member_type, tok);
            member->target = var_expr;
            member->indices.push_back(index);
            member->dereferences.push_back(dereference);

            auto var = new Variable(member_type, tok);
            var->flags |= VAR_PROXY_STRUCT;
            var->name = member_name;
            var->proxy_member = member;
            
            scope->add_proxy(tok, var);
            index++;
        }
    }

    eat_semicolon();
}

Stmt *Parser::parse_func_def(Token *name, u8 flags) {
    scope_push();

	auto stmt = new FunctionDefinition(name);
	stmt->flags = flags;
	current_function = stmt;

    auto cname = name->lexeme;
    stmt->unmangled_name = cname;

    parse_function_parameters(stmt, true);
    auto mname = mangle_func(stmt);
    stmt->mangled_name = mname;

    if (eat_token_if('{')) {
        stmt->return_types.push_back(typer->get("void"));
    } else {
        auto return_type = parse_type();
        stmt->return_types.push_back(return_type);

        while (!eat_token_if('{')) {
            eat_token_if(',');
            stmt->return_types.push_back(parse_type());
        }
    }

    while (!eat_token_if('}')) {
        stmt->body.push_back(parse_stmt());
    }
    scope_pop();

    insert_func(name, mname, stmt);
    return stmt;
}

Stmt *Parser::parse_extern_func_def(Token *name) {
    auto stmt = new FunctionDefinition(name);
	stmt->flags = FUNCTION_EXTERN;

    auto cname = name->lexeme;
    stmt->unmangled_name = cname;

    parse_function_parameters(stmt, false);

    auto mname = mangle_func(stmt);
    stmt->mangled_name = mname;

    if (eat_token_if(';')) {
        stmt->return_types.push_back(typer->get("void"));
    } else {
        auto return_type = parse_type();
        stmt->return_types.push_back(return_type);

        eat_token_if(';');
    }

    insert_func(name, mname, stmt, true);
    return stmt;
}

void Parser::parse_function_parameters(FunctionDefinition *stmt, bool add_to_scope) {
	if (!eat_token_if('(')) {
		messenger->report(lexer.peek_token(), "Expected ( after function name");
	}

	while (!eat_token_if(')')) {
		auto tok = lexer.peek_token();
		
		if (!eat_token_if(TOKEN_ATOM)) {
			messenger->report(lexer.peek_token(), "Expected identifier");
		}

		auto pname = tok->lexeme;

		auto ptype = parse_type();
		if (ptype->ispointer()) {
		    auto elm_type = ptype->element_type;
		    while (elm_type->ispointer()) {
		        elm_type = elm_type->element_type;
            }
            if (elm_type->isarray() && (elm_type->flags & ARRAY_STATIC)) {
                elm_type->flags |= ARRAY_PACKED;
            }
        } else if (ptype->isarray() && (ptype->flags & ARRAY_STATIC)) {
            ptype->flags |= ARRAY_PACKED;
        }

		auto var = new Variable(ptype, tok);
		var->name = pname;
		stmt->parameters.push_back(var);

		if (add_to_scope) {
			scope->add(tok, var);
		}

		eat_token_if(',');

		if (eat_token_if(TOKEN_DOT_DOT)) {
			stmt->flags |= FUNCTION_VARARG;
			if (!eat_token_if(')')) {
				messenger->report(lexer.peek_token(), "Expected ) after vararg ..");
			}
			break;
		}
	}
}

Stmt *Parser::parse_variable_definition(Token *name_token, u8 flags) {
	auto stmt = new VariableDefinition(name_token);

    parse_variable_definition_base(name_token, flags, stmt);

	return stmt;
}

Stmt *Parser::parse_variable_definition_type(Token *name_token, u8 flags) {
    QType *type = parse_type();

    if (eat_token_if(';')) {
        if (type->isarray() && (type->flags & ARRAY_STATIC) && type->array_size == -1) {
            messenger->report(name_token, "Cannot declare variable of static array type with undefined array length");
        }
        
        auto stmt = new VariableDefinition(name_token);
        stmt->var = new Variable(type, name_token);
        stmt->var->name = name_token->lexeme;
        stmt->var->flags |= flags & VAR_CONST;
        stmt->value = 0;
        stmt->flags = flags;

        scope->add(name_token, stmt->var);

        eat_semicolon();
        return stmt;
    }

    if (lexer.peek_token()->type != '=') {
        messenger->report(lexer.peek_token(), "Expected '=' after variable type specifier");
    }

    lexer.eat_token();

    auto stmt = new VariableDefinition(name_token);

    auto val_type = parse_variable_definition_base(name_token, flags, stmt);
    if (!typer->compare(type, val_type)) {
        if (typer->can_convert_implicit(val_type, type)) {
            stmt->value = cast(stmt->value, type);
			stmt->var->type = type;
        } else {
            messenger->report(name_token, "Specified type of variable and type of value do not match");
        }
    }

	return stmt;
}

QType *Parser::parse_variable_definition_base(Token *name_token, u8 flags, VariableDefinition *stmt) {
    auto val_expr = parse_expr();
    auto val_type = val_expr->type;

	stmt->var = new Variable(val_type, name_token);
	stmt->var->name = name_token->lexeme;
    stmt->var->flags |= flags & VAR_CONST;
	stmt->value = val_expr;
    stmt->flags = flags;

    if ((flags & VAR_CONST) && !expr_is_constant(val_expr)) {
        messenger->report(name_token, "Can't assign non-constant value to constant variable");
    }

	scope->add(name_token, stmt->var);

	eat_semicolon();

    return val_type;
}

Stmt *Parser::parse_multiple_variable_definition(Token *name_token) {
    std::vector<Variable *> vars;
    std::vector<Token *> var_names;

    var_names.push_back(name_token);
    while (!eat_token_if(':')) {
        auto tok = lexer.peek_token();
        lexer.eat_token();
        var_names.push_back(tok);
        eat_token_if(',');
    }

    if (!eat_token_if('=')) {
        messenger->report(lexer.peek_token(), "Expected '='");
    }

    auto stmt = new VariableDefinition(name_token); 
    stmt->flags |= VAR_MULTIPLE;

    auto expr_tok = lexer.peek_token();
    auto val = parse_function_call();

    auto return_types = val->target_func_decl->return_types;
    if (var_names.size() > return_types.size()) {
        messenger->report(expr_tok, "Can't assign more variables then function returns values");
    }
    
    for (int i = 0; i < var_names.size(); ++i) {
        vars.push_back(add_or_get_variable(var_names[i], return_types[i]));
    }

    eat_token_if(';');

    stmt->value = val;
    stmt->vars = vars;
    return stmt;
}

Stmt *Parser::parse_block() {
    auto stmt = new CompoundStmt(lexer.peek_token());

    while (!eat_token_if('}')) {
        stmt->stmts.push_back(parse_stmt());
    }

    return stmt;
}

Stmt *Parser::parse_if() {
    auto stmt = new If(lexer.last_token());

    auto cond = parse_expr();
    if (!cond->type->isbool()) {
        cond = make_compare_zero(cond);
    }

    stmt->then = parse_stmt();
    if (eat_token_if(TOKEN_ELSE)) {
        stmt->otherwise = parse_stmt();
    } else {
        stmt->otherwise = 0;
    }
    stmt->cond = cond;

    return stmt;
}

Stmt *Parser::parse_while() {
    auto stmt = new While(lexer.last_token());

    auto cond = parse_expr();
    if (!cond->type->isbool()) {
        cond = make_compare_zero(cond);
    }
    
    stmt->cond = cond;
    stmt->body = parse_stmt();
    return stmt;
}

Stmt *Parser::parse_for() {
	auto var_tok = lexer.peek_token();
	auto stmt = new For(lexer.last_token());

	Variable *var = 0;
	QType *iterator_type = 0;

	scope_push();

	if (lexer.peek_token(1)->type == ':' && lexer.peek_token(2)->type == '=') {
		if (var_tok->type != TOKEN_ATOM) {
			messenger->report(var_tok, "Expected variable name");
		}

		lexer.eat_token();
		lexer.eat_token();
		lexer.eat_token();

		var = new Variable(0, var_tok);
		var->name = var_tok->lexeme;

		scope->add(var_tok, var);
	} else {
		var = new Variable(0, var_tok);
		var->name = "it";
		scope->add_replace(var);
	}

	auto iterator = parse_expr();
	if (eat_token_if(TOKEN_DOT_DOT)) {
		auto size_ty = typer->get("u64");
		auto range_to = parse_expr();
		stmt->range_from = cast(iterator, size_ty);
		stmt->range_to = cast(range_to, size_ty);
		stmt->is_range = true;

		if (!iterator->type->isint() || !range_to->type->isint()) {
			messenger->report(var_tok, "From and range expressions have to be integer expressions!");
		}
		iterator_type = size_ty;
	} else {
		stmt->iterator = iterator;
		stmt->is_range = false;

		if (iterator->type->isstring()) {
			iterator_type = typer->get("char");
		} else if (iterator->type->isarray()) {
			iterator_type = iterator->type->element_type;
		} else {
			messenger->report(var_tok, "Iterator has to be of type array or string");
		}
	}

	var->type = iterator_type;

	auto body = parse_stmt();
	scope_pop();

	stmt->var = var;
	stmt->body = body;

	return stmt;
}

Stmt *Parser::parse_return() {
    auto return_types = current_function->return_types;
    std::vector<Expr *> return_values;
    auto tok = lexer.peek_token();

    if (!eat_token_if(';')) {
        return_values.push_back(parse_expr());

        while (!eat_token_if(';')) {
            eat_token_if(',');
            return_values.push_back(parse_expr());
        }
    }

    if (return_values.size() != return_types.size()) {
        messenger->report(tok, "Return values do not match return types of function");
    } else {
        for (int i = 0; i < return_values.size(); ++i) {
            auto ret_val = return_values[i];
            auto ret_type = return_types[i];

            if (!typer->compare(ret_val->type, ret_type)) {
                if (typer->can_convert_implicit(ret_val->type, ret_type)) {
                    return_values[i] = cast(ret_val, ret_type);
                } else {
                    messenger->report(tok, "Return values do not match return types of function");
                }
            }
        }
    }

    auto stmt = new Return(tok);
	stmt->return_values = return_values;

	return stmt;
}

Stmt *Parser::parse_delete() {
    auto stmt = new Delete(lexer.last_token());
    auto tok = lexer.peek_token();
    stmt->target_expr = parse_expr();

	auto target_ty = stmt->target_expr->type;
    if (!target_ty->ispointer() && !target_ty->isarray()) {
        messenger->report(tok, "Can't delete non-pointer");
    }
    if (target_ty->isarray() && (target_ty->flags & ARRAY_STATIC)) {
        messenger->report(tok, "Can't delete static array");
    }
    
    eat_semicolon();

    return stmt;
}

Stmt *Parser::try_parse_atom() {
	auto atom = lexer.peek_token();

	if (atom->type == TOKEN_ATOM) {
        if (lexer.peek_token(1)->type == ':') {
            lexer.eat_token();
            lexer.eat_token();

            if (eat_token_if('=')) {
                return parse_variable_definition(atom, 0);
            } else if (eat_token_if('@')) {
                return parse_variable_definition(atom, VAR_CONST);
            } else {
                return parse_variable_definition_type(atom, false);
            }
        } else if (lexer.peek_token(1)->type == ',') {
            lexer.eat_token();
            lexer.eat_token();

            return parse_multiple_variable_definition(atom);
        }
	}

    auto stmt = new ExprStmt(atom);
	stmt->target_expr = parse_expr();
    eat_semicolon();

    return stmt;
}

Expr *Parser::parse_expr(int prec) {
	return parse_binary(prec);
}

Expr *Parser::parse_binary(int prec) {
	auto lhs = parse_cast();

	while (true) {
		auto tok = lexer.peek_token();
		auto tok_type = tok->type;
		auto op_it = operator_precedence.find(tok_type);

		if (op_it == operator_precedence.end())
			break;
		if (op_it->second < prec)
			break;

		lexer.eat_token();
        auto rhs = parse_binary(op_it->second + 1);
        auto lhs_type = lhs->type;
        auto rhs_type = rhs->type;
        auto eval_type = lhs_type;

        if (ttype_is_conditional(tok_type)) {
            eval_type = typer->get("bool");
            
            if (lhs_type->isstring() && rhs_type->isstring()) {
                if (tok_type == TOKEN_EQ_EQ || tok_type == TOKEN_NOT_EQ) {
                    lhs = make_compare_strings(lhs, rhs, tok_type); 
                    continue;
                } else {
                    messenger->report(tok, "Illegal operator for string types");
                }
            }
        }

        if (ttype_is_logical(tok_type)) {
            eval_type = typer->get("bool");

            if (!lhs_type->isbool()) {
                lhs = make_compare_zero(lhs);
            }
            if (!rhs_type->isbool()) {
                rhs = make_compare_zero(rhs);
            }
        }

        if (ttype_is_assign(tok_type)) {
            if (!expr_is_targatable(lhs)) {
                messenger->report(tok, "Can't assign value to this expression");
            }
            if (lhs->kind() == VARIABLE) {
                auto lhs_var = (Variable *) lhs;
                if (lhs_var->flags & VAR_CONST) {
                    messenger->report(tok, "Can't assign value to constant variable");
                }
            }
        }

        if (ttype_is_binary(tok_type)) {
            if (!(lhs_type->ispointer() && rhs_type->isint()) &&
				(!lhs_type->isint() && !rhs_type->isint()) &&
				(!lhs_type->isfloat() && !rhs_type->isfloat())) {
				if (lhs_type->ispointer() && (tok_type != '+' && tok_type != '-')) {
				    messenger->report(tok, "Illegal binary operator for pointer type!");
                }
                messenger->report(tok, "Illegal type for binary expression"); 
            }
        }

        if (!typer->compare(lhs_type, rhs_type)) {
            if (typer->can_convert_implicit(rhs_type, lhs_type)) {
                rhs = cast(rhs, lhs_type);
            } else {
                if (!(lhs_type->ispointer() && rhs_type->isint() && ttype_is_binary(tok_type))) {
					messenger->report(tok, "Lhs and Rhs of binary expression are of different types");
                }
            }
        }

        auto expr = new Binary(eval_type, tok);
        expr->lhs = lhs;
        expr->rhs = rhs;
        expr->op = op_it->first;

        lhs = expr;
	}

	return lhs;
}

Expr *Parser::parse_cast() {
	auto expr = parse_unary();

	if (eat_token_if(TOKEN_AS)) {
		auto tok = lexer.peek_token();
		auto type = parse_type();

		if (!typer->can_convert_explicit(expr->type, type)) {
			messenger->report(tok, "Invalid cast");
		}

		return cast(expr, type);
	}

	return expr;
}

Expr *Parser::parse_unary() {
    auto tok = lexer.peek_token();

    if (eat_token_if('&')) {
        auto target = parse_postfix();
        auto target_kind = target->kind();

        if (target_kind != VARIABLE &&
            target_kind != MEMBER &&
            target_kind != INDEXED) {
            messenger->report(tok, "Can't reference non variable");
        }

        auto target_type = target->type;

		auto expr = new Unary(typer->make_pointer(target_type), tok); 
        expr->target = target;
        expr->op = '&';

        return expr;
    } else if (eat_token_if('*')) {
        auto target = parse_unary();

        if (!expr_is_targatable(target)) {
            messenger->report(tok, "Can't dereference non variable");
        }

		auto expr = new Deref(target->type->element_type, tok);
        expr->target = target;
		
        return expr;
    } else if (eat_token_if('+')) {
        return parse_postfix();
    } else if (eat_token_if('!')) {
        auto target = parse_postfix();
        if (!target->type->isbool()) {
            target = make_compare_zero(target);
        }

		auto expr = new Unary(target->type, tok);
        expr->target = target;
        expr->op = '!';
		
        return expr;
    } else if (eat_token_if('-')) {
        auto target = parse_postfix();

		auto expr = new Unary(target->type, tok);
        expr->target = target;
        expr->op = '-';
		
        return expr;
	} else if (eat_token_if(TOKEN_PLUS_PLUS) || eat_token_if(TOKEN_MINUS_MINUS)) {
		auto target = parse_postfix();
		if (!target->type->isint()) {
			messenger->report(tok, "Can't use ++ or -- on non-integer expression");
		}

		auto expr = new Unary(target->type, tok);
		expr->target = target;
		expr->op = tok->type;
		expr->ispost = false;
		return expr;
	}

	return parse_postfix();
}

Expr *Parser::parse_postfix() {
	auto target = parse_access();
	auto tok = lexer.peek_token();

	if (eat_token_if(TOKEN_PLUS_PLUS) || eat_token_if(TOKEN_MINUS_MINUS)) {
		if (!target->type->isint()) {
			messenger->report(tok, "Can't use ++ or -- on non-integer expression");
		}

		auto expr = new Unary(target->type, tok);
		expr->target = target;
		expr->op = tok->type;
		expr->ispost = true;
		return expr;
	}

    if (eat_token_if('[')) {
        if (!target->type->ispointer() && !target->type->isarray() && !target->type->isstring()) {
            messenger->report(tok, "This type can't be indexed");
        }

        auto index = parse_expr();
        eat_token_if(']');

        if (!index->type->isint()) {
            messenger->report(tok, "Index has to be of integer type");
        }

        auto indexed = new Indexed(target->type->element_type, tok);
        indexed->index = index;
        indexed->target = target;

        return indexed;
    }
	
	return target;
}
Expr *Parser::parse_access() {
    auto e = parse_primary();

    if (!expr_is_targatable(e) || lexer.peek_token()->type != '.') {
        return e;
    }

    auto ty = e->type;

    std::vector<int> indices;
    std::vector<bool> dereferences;

    while (eat_token_if('.')) {
        bool dereference = false;

        if (ty->isstruct()) {
            ;
        } else if (ty->ispointer() && ty->element_type->isstruct()) {
            dereference = true;
            ty = ty->element_type;
        } else {
            messenger->report(lexer.peek_token(), "Expected struct type before '.'");
        }

        auto mem_token = lexer.peek_token();
        if (!eat_token_if(TOKEN_ATOM)) {
            messenger->report(mem_token, "Expected identifier");
        }

        int i = 0;
        bool found = false;
        for (auto field : *ty->fields) {
            if (strcmp(field.first, mem_token->lexeme) == 0) {
                dereferences.push_back(dereference);
                indices.push_back(i);
                ty = field.second;
                found = true;
                break;
            }
            i++;
        }
        if (!found) {
            messenger->report(mem_token, "Member with that name doesn't exist");
        }
    }

    auto expr = new Member(ty, lexer.last_token());
    expr->target = e;
    expr->indices = indices;
    expr->dereferences = dereferences;
    return expr;
}

Expr *Parser::parse_primary() {
	auto tok = lexer.peek_token();

	if (eat_token_if('(')) {
		auto expr = parse_expr();
		if (!eat_token_if(')')) {
			messenger->report(lexer.peek_token(), "Expected ')'");
		}
		return expr;
	}

	if (tok->type == TOKEN_INT_LIT) {
		lexer.eat_token();
		
		auto expr = new IntegerLiteral(typer->get("s32"), tok);
        expr->int_value = tok->int_value;

        return expr;
	}

	if (tok->type == TOKEN_FLOAT_LIT) {
		lexer.eat_token();

		auto expr = new FloatLiteral(typer->get("f32"), tok);
		expr->float_value = tok->float_value;

		return expr;
	}

	if (tok->type == TOKEN_CHAR_LIT) {
		lexer.eat_token();
		
		auto expr = new IntegerLiteral(typer->get("char"), tok);
        expr->int_value = tok->char_value;

        return expr;
	}

    if (tok->type == TOKEN_STRING_LIT) {
		lexer.eat_token();
		
		auto expr = new QStringLiteral(typer->get("str"), tok);
        expr->string_lit = tok->lexeme;

        return expr;
    }

	if (tok->type == TOKEN_ATOM) {
		lexer.eat_token();

        if (eat_token_if('(')) {
            std::vector<Expr *> arguments;

            while (!eat_token_if(')')) {
                auto arg = parse_expr();
                arguments.push_back(arg);

                eat_token_if(',');
            }

            auto unmangled_name = tok->lexeme;
			if (is_builtin(unmangled_name)) {
				auto return_type = get_builtin_return_type(unmangled_name);
				auto expr = new Builtin(return_type, tok);
				expr->name = unmangled_name;
				expr->arguments = arguments;
				check_builtin_func(tok, expr);
				return expr;
			} else {
				auto func_decl = get_func(tok, &arguments);

				auto return_types = func_decl->return_types;
				auto expr = new FunctionCall(return_types[0], tok);
				expr->arguments = arguments;
				expr->target_func_decl = func_decl;
				return expr;
			}
        }

        if (eat_token_if(TOKEN_COLON_COLON)) {
            auto ty = typer->get(tok->lexeme);
            if (!ty->isenum()) {
                messenger->report(tok, "Can't access non enum type by <Type>.");
            }

            auto category = lexer.peek_token();
            if (!eat_token_if(TOKEN_ATOM)) {
                messenger->report(category, "Expected identifier token to access enum");
            }

            auto categories = *ty->categories;
            for (int i = 0; i < categories.size(); ++i) {
                if (strcmp(categories[i], category->lexeme) == 0) {
                    auto expr = new IntegerLiteral(typer->get("u64"), tok);
                    expr->int_value = (*ty->indices)[i];
                    return expr;
                }
            }

            messenger->report(category, "Value not found in enum");
        }

		return scope->find(tok);
	}

	if (eat_token_if('{')) {
        std::vector<Expr *> values;
        bool lit_is_constant = true;
		while (!eat_token_if('}')) {
		    auto val = parse_expr();
            if (!expr_is_constant(val)) {
                lit_is_constant = false;
            }
		    
			values.push_back(val);
			eat_token_if(',');
		}

		auto ty_tok = lexer.peek_token();
		auto ty = parse_type();
		if (ty->isstruct()) {
			if (values.size() != ty->fields->size()) {
				messenger->report(ty_tok, "Compound literal do not match struct type");
			}
			for (int i = 0; i < values.size(); ++i) {
				auto val_type = values[i]->type;
				auto field_type = (*ty->fields)[i].second;
				if (!typer->compare(val_type, field_type)) {
					if (typer->can_convert_implicit(val_type, field_type)) {
						values[i] = cast(values[i], field_type);
					} else {
						messenger->report(ty_tok, "Compound literal does not match struct type");
					}
				}
			}
		} else if (ty->isarray()) {
			auto arr_type = ty->element_type;
			
            if (ty->flags & ARRAY_STATIC) {
                if (ty->array_size == -1) {
                    ty = typer->make_array(ty->element_type, ARRAY_STATIC, values.size());
                } else if (ty->array_size != values.size()) {
                    messenger->report(ty_tok, "Number of values in compound literal does not match array type");
                }
            }
            
            for (int i = 0; i < values.size(); ++i) {
				auto val_type = values[i]->type;
				if (!typer->compare(val_type, arr_type)) {
					if (typer->can_convert_implicit(val_type, arr_type)) {
						values[i] = cast(values[i], arr_type);
					} else {
						messenger->report(ty_tok, "Compound literal does not match array type");
					}
				}
			}
		} else {
			messenger->report(ty_tok, "Illegal type for compound literal.\nExpected struct or array type!");
		}

		auto expr = new CompoundLiteral(ty, tok);
		expr->values = values;
		expr->lit_is_constant = lit_is_constant;
		return expr;
	}

	if (eat_token_if(TOKEN_NEW)) {
	    auto ty = parse_type();
	    auto expr = new New(typer->make_pointer(ty), tok);
	    expr->alloc_type = ty;
	    return expr;
    }

	if (eat_token_if(TOKEN_TRUE)) {
	    auto expr = new IntegerLiteral(typer->get("bool"), tok);
	    expr->int_value = 1;
	    return expr;
    }

	if (eat_token_if(TOKEN_FALSE)) {
	    auto expr = new IntegerLiteral(typer->get("bool"), tok);
	    expr->int_value = 0;
	    return expr;
    }

	if (eat_token_if(TOKEN_NIL)) {
	    auto expr = new Nil(typer->make_nil(), tok);
	    return expr;
    }

    if (eat_token_if(TOKEN_SIZEOF)) {
        auto expr = new SizeOf(typer->get("u64"), tok);
        expr->target_type = parse_type();
        return expr;
    }

	messenger->report_print_token(tok, "Unexpected token");
	return 0;
}

FunctionCall *Parser::parse_function_call() {
    auto tok = lexer.peek_token();
	if (!eat_token_if(TOKEN_ATOM)) {
	    messenger->report(lexer.peek_token(), "Expected function call!");
    }

    if (!eat_token_if('(')) {
	    messenger->report(lexer.peek_token(), "Expected function call!");
    }

    std::vector<Expr *> arguments;

    while (!eat_token_if(')')) {
        auto arg = parse_expr();
        arguments.push_back(arg);

        eat_token_if(',');
    }

    auto unmangled_name = tok->lexeme;
    auto func_decl = get_func(tok, &arguments);

    auto return_types = func_decl->return_types;
    auto expr = new FunctionCall(return_types[0], tok);
    expr->arguments = arguments;
    expr->target_func_decl = func_decl;
    return expr;
}

Expr *Parser::cast(Expr *target, QType *to) {
	if (typer->compare(target->type, to))
		return target;

    auto expr = new Cast(to, target->location);
    expr->from = target->type;
    expr->to = to;
    expr->target = target;

    return expr;
}

Expr *Parser::make_compare_zero(Expr *expr) {
    auto cmp_expr = new CompareZero(typer->get("bool"), expr->location);
    cmp_expr->target = expr;
    return cmp_expr;
}

Expr *Parser::make_compare_strings(Expr *lhs, Expr *rhs, TokenType op) {
    std::vector<Expr *> arguments;

    arguments.push_back(lhs);
    arguments.push_back(rhs);

    auto fn = get_func("strcmp_s_s");

    auto fn_call_expr = new FunctionCall(typer->get("s32"), lhs->location);
    fn_call_expr->arguments = arguments;
    fn_call_expr->target_func_decl = fn;

    auto zero = new IntegerLiteral(typer->get("s32"), lhs->location);
    zero->int_value = 0;
    
    auto cmp_expr = new Binary(typer->get("bool"), lhs->location);
    cmp_expr->lhs = fn_call_expr;
    cmp_expr->rhs = zero;
    cmp_expr->op = op;

    return cmp_expr;
}

bool Parser::expr_is_targatable(Expr *expr) {
    ExprKind kind = expr->kind();
    switch (kind) {
        case VARIABLE: {
            auto var = (Variable *) expr;
            return !(var->flags & VAR_PROXY_ENUM);
        }
        case DEREF:
        case MEMBER:
        case INDEXED:
            return true;
        default:
            return false;
    }
}	

bool Parser::expr_is_constant(Expr *expr) {
    ExprKind kind = expr->kind();
    switch (kind) {
        case INT_LIT:
        case FLOAT_LIT:
        case STRING_LIT:
        case COMPOUND_LIT:
            return true;
        case VARIABLE: {
            auto var = (Variable *) expr;
            return var->flags & VAR_CONST;
        }
        default:
            return false;
    }
}	

QType *Parser::parse_type() {
	auto tok = lexer.peek_token();
	if (tok->type == '*') {
		lexer.eat_token();
		return typer->make_pointer(parse_type());
	}
    if (tok->type == '[') {
        lexer.eat_token();
        
        u8 flags = 0;
        long int array_size = -1;
        
        if (eat_token_if(TOKEN_DOT_DOT)) {
            flags |= ARRAY_DYNAMIC;
        } else {
            flags |= ARRAY_STATIC;
            auto size_token = lexer.peek_token();
            if (eat_token_if(TOKEN_INT_LIT)) {
                array_size = size_token->int_value;
            }
        }
        
        if (!eat_token_if(']')) {
            messenger->report(lexer.peek_token(), "Expected ']'");
        }
        
        return typer->make_array(parse_type(), flags, array_size);
	}
	if (tok->type != TOKEN_ATOM) {
		messenger->report(tok, "Expected type");
	}
	lexer.eat_token();
	return typer->get(tok);
}

bool Parser::token_is_op(char op, int off) {
	auto tok = lexer.peek_token(off);
	return tok->type == op;
}

bool Parser::eat_token_if(TokenType type) {
	auto tok = lexer.peek_token();
	if (tok->type == type) {
		lexer.eat_token();
		return true;
	}
	return false;
}

bool Parser::eat_token_if(char type) {
	return eat_token_if((TokenType) type);
}

void Parser::eat_semicolon() {
	eat_token_if(';');
}

void Parser::scope_push() {
	scope = new Scope(scope->messenger, scope);
}

void Parser::scope_pop() {
	assert(scope->parent);
	scope = scope->parent;
}

const char *Parser::mangle_func(FunctionDefinition *stmt) {
    auto unmangled_name = stmt->unmangled_name;
	if (strcmp(unmangled_name, "main") == 0 ||
		stmt->flags & FUNCTION_BUILTIN) {
		return unmangled_name;
	}
	std::stringstream ss;
	ss << unmangled_name;
	for (auto par : stmt->parameters) {
		ss << "_" << typer->mangle_type(par->type);
	}

    if (stmt->flags & FUNCTION_VARARG)
        ss << "vararg";

	std::string str = ss.str();
	char *mangled = new char[str.length() + 1];
	std::strcpy(mangled, str.c_str());
	return mangled;
}

Scope::Scope(Messenger *_messenger, Scope *_parent) {
	messenger = _messenger;
	parent = _parent;
}

void Scope::add(Token *token, Variable *var) {
	auto sname = std::string(token->lexeme);
	if (variables.find(sname) != variables.end()) {
		messenger->report(token, "Variable already defined");
	}
	variables.insert(std::make_pair(sname, var));
}

void Scope::add_proxy(Token *token, Variable *var) {
	auto sname = std::string(var->name);
	if (variables.find(sname) != variables.end()) {
		messenger->report(token, "Illegal 'using'. Variable with member name already defined");
	}
	variables.insert(std::make_pair(sname, var));
}

void Scope::add_replace(Variable *var) {
	auto sname = std::string(var->name);
	variables[sname] = var;
}

Variable *Scope::find(Token *token) {
    auto var = find_null(token->lexeme);
    if (!var) {
        messenger->report(token, "Undefined variable");
    }
    return var;
}

Variable *Scope::find_null(const char *name) {
	auto sname = std::string(name);
	auto it = variables.find(sname);
	if (it == variables.end()) {
		if (parent) {
			return parent->find_null(name);
		}
        return 0;
	}
	return it->second;
}

void Parser::insert_func(Token *name_token, const char *mangled_name, FunctionDefinition *func_decl, bool is_extern) {
    auto sname = std::string(mangled_name);
    auto it = functions.find(sname);
    if (it != functions.end()) {
        if (!is_extern) {
            messenger->report(name_token, "Function already exists");
        }
    } else {
        functions.insert(std::make_pair(sname, func_decl));
    }
}

FunctionDefinition *Parser::get_func(Token *name_token, std::vector<Expr *> *arguments) {
    auto unmangled_name = name_token->lexeme;

    for (auto key : functions) {
        auto decl = key.second;
        if (strcmp(decl->unmangled_name, unmangled_name) != 0)
            continue;

        auto decl_args = decl->parameters;
        int decl_ac = decl_args.size();
        int call_ac = arguments->size();
        bool vararg = decl->flags & FUNCTION_VARARG;

        if (call_ac < decl_ac || (call_ac > decl_ac && !vararg))
            continue;

        bool matching = true;
        for (int i = 0; i < decl_ac; ++i) {
            auto arg = (*arguments)[i];
            QType *arg_type = arg->type;
            QType *par_type = decl_args[i]->type;

            if (!typer->compare(arg_type, par_type)) {
                if (typer->can_convert_implicit(arg_type, par_type)) {
                    (*arguments)[i] = cast(arg, par_type);
                } else {
                    matching = false;
                    break;
                }
            }
        }

        if (matching) {
            return decl;
        }
    }

    messenger->report(name_token, "Function does not exists");
    return 0;
}

FunctionDefinition *Parser::get_func(const char *name) {
    auto sname = std::string(name);
    return functions[sname];
}

Variable *Parser::add_or_get_variable(Token *token, QType *type) {
    Variable *var = scope->find_null(token->lexeme);
    if (var) {
        if (!typer->compare(var->type, type)) {
            messenger->report(token, "Variable already defined");
        }
        return var;
    }

    var = new Variable(type, token->location);
	var->name = token->lexeme;
	var->type = type;
	scope->add(token, var);
    return var;
}

void init_preproc_definitions() {
#if defined(_WIN32) || defined(_WIN64)
    preproc_definitions.push_back("windows");
#elif defined(__APPLE__) || defined(__MACH__)
    preproc_definitions.push_back("unix");
    preproc_definitions.push_back("macos");
#elif defined(__linux__)
    preproc_definitions.push_back("unix");
    preproc_definitions.push_back("linux");
#endif
}
