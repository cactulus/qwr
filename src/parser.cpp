#include <cassert>
#include <iostream>
#include <sstream>
#include <cstring>

#include "parser.h"
#include "arena.h"

const std::unordered_map<int, int> operator_precedence = {
	{TOKEN_ADD_EQ, 1},
	{TOKEN_SUB_EQ, 1},
	{TOKEN_MUL_EQ, 1},
	{TOKEN_DIV_EQ, 1},
	{TOKEN_MOD_EQ, 1},
	{'=', 1},
    {TOKEN_BAR_BAR, 2},
    {TOKEN_AND_AND, 3},
    {TOKEN_EQ_EQ, 4},
    {TOKEN_NOT_EQ, 4},
    {TOKEN_LT_EQ, 5},
    {TOKEN_GT_EQ, 5},
    {'<', 5},
    {'>', 5},
	{'+', 6},
	{'-', 6},
	{'*', 7},
	{'/', 7},
	{'%', 7},
};

static Stmt *current_function;

void Parser::init(Typer *_typer, Messenger *_messenger) {
	typer = _typer;
	messenger = _messenger;

	scope = new Scope(messenger);
	has_reached_end = false;
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
                return parse_top_level_stmt();
            } else if (eat_token_if(TOKEN_STRUCT)) {
                parse_struct(tok);
                return parse_top_level_stmt();
            } else {
                return parse_func_def(tok);
            }
        } else {
            messenger->report(lexer.peek_token(2), "Unexpected token");
        }
    }

    if (eat_token_if(TOKEN_EXTERN)) {
        tok = lexer.peek_token();
        if (tok->type == TOKEN_ATOM) {
            auto pt = lexer.peek_token(1)->type;
            if (pt == TOKEN_COLON_COLON) {
                lexer.eat_token();
                lexer.eat_token();
                return parse_extern_func_def(tok);
            } else {
                messenger->report(lexer.peek_token(2), "Unexpected token");
            }
        } else {
            messenger->report(tok, "Expected function name after 'extern'");
        }
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

	if (eat_token_if(TOKEN_RETURN)) {
		return parse_return();
	}

	if (eat_token_if(TOKEN_DELETE)) {
		return parse_delete();
	}

	return try_parse_atom();
}

void Parser::parse_enum(Token *name) {
	if (!eat_token_if('{')) {
		messenger->report(lexer.peek_token(), "Expected { after enum name");
	}
     
    auto type = typer->make_type(TYPE_ENUM, 0);
    typer->insert_custom(name, type);
    type->categories = new std::vector<const char *>();

    while (!eat_token_if('}')) {
        auto tok = lexer.peek_token();
        if (!eat_token_if(TOKEN_ATOM)) {
            messenger->report(tok, "Only identifiers allowed in enum definition");
        }

        type->categories->push_back(tok->lexeme);

        eat_token_if(',');
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

		if (!eat_token_if(':')) {
			messenger->report(lexer.peek_token(), "Expected : after struct field name");
		}

        auto fty = parse_type();

        fields->push_back(std::make_pair(tok->lexeme, fty));

        eat_token_if(',');
    }

    auto type = typer->make_struct(name->lexeme, fields);
    typer->insert_custom(name, type);
}

Stmt *Parser::parse_func_def(Token *name) {
    scope_push();

	auto stmt = make_stmt(FUNCTION_DEFINITION);
	current_function = stmt;

    auto cname = name->lexeme;
    stmt->func_def.unmangled_name = cname;

    parse_function_parameters(stmt, true);
    auto mname = mangle_func(stmt);
    stmt->func_def.mangled_name = mname;

    stmt->func_def.return_types = new std::vector<QType *>();

    if (eat_token_if('{')) {
        stmt->func_def.return_types->push_back(typer->get("void"));
    } else {
        auto return_type = parse_type();
        stmt->func_def.return_types->push_back(return_type);

        while (!eat_token_if('{')) {
            eat_token_if(',');
            stmt->func_def.return_types->push_back(parse_type());
        }
    }

    stmt->func_def.body = new std::vector<Stmt *>();

    while (!eat_token_if('}')) {
        stmt->func_def.body->push_back(parse_stmt());
    }
    scope_pop();

    insert_func(name, mname, stmt);
    return stmt;
}

Stmt *Parser::parse_extern_func_def(Token *name) {
    auto stmt = make_stmt(EXTERN_FUNCTION);

    auto cname = name->lexeme;
    stmt->func_def.unmangled_name = cname;

    parse_function_parameters(stmt, false);

    auto mname = mangle_func(stmt);
    stmt->func_def.mangled_name = mname;
    stmt->func_def.return_types = new std::vector<QType *>();

    if (eat_token_if(';')) {
        stmt->func_def.return_types->push_back(typer->get("void"));
    } else {
        auto return_type = parse_type();
        stmt->func_def.return_types->push_back(return_type);

        eat_token_if(';');
    }

    insert_func(name, mname, stmt);
    return stmt;
}

void Parser::parse_function_parameters(Stmt *stmt, bool add_to_scope) {
	if (!eat_token_if('(')) {
		messenger->report(lexer.peek_token(), "Expected ( after function name");
	}

    stmt->func_def.isvararg = false;
    stmt->func_def.parameters = new std::vector<Variable *>();

	while (!eat_token_if(')')) {
		auto tok = lexer.peek_token();
		
		if (!eat_token_if(TOKEN_ATOM)) {
			messenger->report(lexer.peek_token(), "Expected identifier");
		}

		auto pname = tok->lexeme;
		if (!eat_token_if(':')) {
			messenger->report(lexer.peek_token(), "Expected : after parameter name");
		}

		auto ptype = parse_type();
		auto var = make_variable(pname, ptype);
		stmt->func_def.parameters->push_back(var);

		if (add_to_scope) {
			scope->add(tok, var);
		}

		eat_token_if(',');

		if (eat_token_if(TOKEN_DOT_DOT)) {
			stmt->func_def.isvararg = true;
			if (!eat_token_if(')')) {
				messenger->report(lexer.peek_token(), "Expected ) after vararg ..");
			}
			break;
		}
	}
}

Stmt *Parser::parse_variable_definition(Token *name_token, u8 flags) {
	auto stmt = make_stmt(VARIABLE_DEFINITION);

    parse_variable_definition_base(name_token, flags, stmt);

	return stmt;
}

Stmt *Parser::parse_variable_definition_type(Token *name_token, u8 flags) {
    QType *type = parse_type();

    if (eat_token_if(';')) {
        auto stmt = make_stmt(VARIABLE_DEFINITION);
        stmt->var_def.var = make_variable(name_token->lexeme, type);
        stmt->var_def.var->is_const = flags & VAR_CONST;
        stmt->var_def.value = 0;
        stmt->var_def.flags = flags;

        scope->add(name_token, stmt->var_def.var);

        eat_semicolon();
        return stmt;
    }

    if (lexer.peek_token()->type != '=') {
        messenger->report(lexer.peek_token(), "Expected '=' after variable type specifier");
    }

    lexer.eat_token();

    auto stmt = make_stmt(VARIABLE_DEFINITION);

    auto val_type = parse_variable_definition_base(name_token, flags, stmt);
    if (!typer->compare(type, val_type)) {
        if (typer-> can_convert(val_type, type)) {
            stmt->var_def.value = cast(stmt->var_def.value, type);
			stmt->var_def.var->type = type;
        } else {
            messenger->report(name_token, "Specified type of variable and type of value do not match");
        }
    }

	return stmt;
}

QType *Parser::parse_variable_definition_base(Token *name_token, u8 flags, Stmt *stmt) {
    auto val_expr = parse_expr();
    auto val_type = val_expr->type;

	stmt->var_def.var = make_variable(name_token->lexeme, val_type);
	stmt->var_def.var->is_const = flags & VAR_CONST;
	stmt->var_def.value = val_expr;
    stmt->var_def.flags = flags;

	scope->add(name_token, stmt->var_def.var);

	eat_semicolon();

    return val_type;
}

Stmt *Parser::parse_multiple_variable_definition(Token *name_token) {
    auto vars = new std::vector<Variable *>();
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

    auto stmt = make_stmt(VARIABLE_DEFINITION); 
    stmt->var_def.flags |= VAR_MULTIPLE;
    stmt->var_def.vars = vars;

    auto expr_tok = lexer.peek_token();
    auto val = parse_expr();
    if (val->kind != FUNCTION_CALL) {
        messenger->report(expr_tok, "Expected function call with multiple return values"); 
    }
    auto return_types = val->func_call.target_func_decl->func_def.return_types;
    if (var_names.size() > return_types->size()) {
        messenger->report(expr_tok, "Can't assign more variables then function returns values");
    }
    
    for (int i = 0; i < var_names.size(); ++i) {
        vars->push_back(add_or_get_variable(var_names[i], (*return_types)[i]));
    }

    eat_token_if(';');

    stmt->var_def.value = val;
    return stmt;
}

Stmt *Parser::parse_block() {
    auto stmt = make_stmt(BLOCK);
    stmt->stmts = new std::vector<Stmt *>();

    while (!eat_token_if('}')) {
        stmt->stmts->push_back(parse_stmt());
    }

    return stmt;
}

Stmt *Parser::parse_if() {
    auto stmt = make_stmt(IF);

    auto cond = parse_expr();
    if (!cond->type->isbool()) {
        cond = make_compare_zero(cond);
    }

    stmt->if_.then = parse_stmt();
    if (eat_token_if(TOKEN_ELSE)) {
        stmt->if_.otherwise = parse_stmt();
    } else {
        stmt->if_.otherwise = 0;
    }
    stmt->if_.cond = cond;

    return stmt;
}

Stmt *Parser::parse_while() {
    auto stmt = make_stmt(WHILE);

    auto cond = parse_expr();
    if (!cond->type->isbool()) {
        cond = make_compare_zero(cond);
    }
    
    stmt->while_.cond = cond;
    stmt->while_.body = parse_stmt();
    return stmt;
}

Stmt *Parser::parse_return() {
    auto return_types = current_function->func_def.return_types;
    std::vector<Expr *> *return_values;
    auto tok = lexer.peek_token();

    if (eat_token_if(';')) {
        return_values = 0;
    } else {
        return_values = new std::vector<Expr *>();

        return_values->push_back(parse_expr());

        while (!eat_token_if(';')) {
            eat_token_if(',');
            return_values->push_back(parse_expr());
        }
    }

    if (return_values->size() != return_types->size()) {
        messenger->report(tok, "Return values do not match return types of function");
    } else {
        for (int i = 0; i < return_values->size(); ++i) {
            auto ret_val = (*return_values)[i];
            auto ret_type = (*return_types)[i];

            if (!typer->compare(ret_val->type, ret_type)) {
                if (typer->can_convert(ret_val->type, ret_type)) {
                    (*return_values)[i] = cast(ret_val, ret_type);
                } else {
                    messenger->report(tok, "Return values do not match return types of function");
                }
            }
        }
    }

	auto stmt = make_stmt(RETURN);
	stmt->return_values = return_values;

	return stmt;
}

Stmt *Parser::parse_delete() {
    auto stmt = make_stmt(DELETE);
    auto tok = lexer.peek_token();
    stmt->target_expr = parse_expr();

    if (!stmt->target_expr->type->ispointer()) {
        messenger->report(tok, "Can't delete non-pointer");
    }
    
    eat_token_if(';');

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

    auto stmt = make_stmt(EXPR_STMT);
    stmt->target_expr = parse_expr();
    eat_semicolon();

    return stmt;
}

Expr *Parser::parse_expr(int prec) {
	return parse_binary(prec);
}

Expr *Parser::parse_binary(int prec) {
	auto lhs = parse_access();

	while (true) {
		auto tok = lexer.peek_token();
		auto tok_type = tok->type;
		auto op_it = operator_precedence.find(tok_type);

		if (op_it == operator_precedence.end())
			break;
		if (op_it->second < prec)
			break;

		lexer.eat_token();
        auto rhs = parse_binary(prec + 1);
        auto lhs_type = lhs->type;
        auto rhs_type = rhs->type;
        auto eval_type = lhs_type;

        if (ttype_is_conditional(tok_type)) {
            eval_type = typer->get("bool");
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
            if (lhs->kind == VARIABLE) {
                if (lhs->var->is_const) {
                    messenger->report(tok, "Can't assign value to constant variable");
                }
            }
        }

        if (ttype_is_binary(tok_type)) {
            if (!(lhs_type->ispointer() && rhs_type->isint()) && (!lhs_type->isint() && !rhs_type->isint())) {
                messenger->report(tok, "Illegal type for binary expression"); 
            }
        }

        if (!typer->compare(lhs_type, rhs_type)) {
            if (typer->can_convert(rhs_type, lhs_type)) {
                rhs = cast(rhs, lhs_type);
            } else {
                if (!(lhs_type->ispointer() && rhs_type->isint() && ttype_is_binary(tok_type))) {
                    messenger->report(tok, "Lhs and Rhs of binary expression are of different types");
                }
            }
        }

        auto expr = make_expr(BINARY, eval_type);
        expr->bin.lhs = lhs;
        expr->bin.rhs = rhs;
        expr->bin.op = op_it->first;

        lhs = expr;
	}

	return lhs;
}

Expr *Parser::parse_access() {
    auto e = parse_unary();

    if (!expr_is_targatable(e) || lexer.peek_token()->type != '.') {
        return e;
    }

    auto ty = e->type;

    auto indices = new std::vector<int>();
    auto dereferences = new std::vector<bool>();

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
                dereferences->push_back(dereference);
                indices->push_back(i);
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

    auto expr = make_expr(MEMBER, ty);
    expr->member.target = e;
    expr->member.indices = indices;
    expr->member.dereferences = dereferences;
    return expr;
}

Expr *Parser::parse_unary() {
    auto tok = lexer.peek_token();

    if (eat_token_if('&')) {
        auto target = parse_postfix();
        auto target_kind = target->kind;

        if (target_kind != VARIABLE) {
            messenger->report(tok, "Can't reference non variable");
        }

        auto target_type = target->type;

		auto expr = make_expr(UNARY, typer->make_pointer(target_type)); 
        expr->unary.target = target;
        expr->unary.op = '&';

        return expr;
    } else if (eat_token_if('*')) {
        auto target = parse_unary();

        if (!expr_is_targatable(target)) {
            messenger->report(tok, "Can't dereference non variable");
        }

		auto expr = make_expr(DEREF, target->type->element_type);
        expr->target = target;
		
        return expr;
    } else if (eat_token_if('+')) {
        return parse_postfix();
    } else if (eat_token_if('!')) {
        auto target = parse_postfix();
        if (!target->type->isbool()) {
            target = make_compare_zero(target);
        }

		auto expr = make_expr(UNARY, target->type);
        expr->unary.target = target;
        expr->unary.op = '!';
		
        return expr;
    } else if (eat_token_if('-')) {
        auto target = parse_postfix();

		auto expr = make_expr(UNARY, target->type);
        expr->unary.target = target;
        expr->unary.op = '-';
		
        return expr;
    }

    return parse_postfix();
}

Expr *Parser::parse_postfix() {
    auto expr = parse_primary();

    if (eat_token_if(TOKEN_AS)) {
        auto type = parse_type();

        return cast(expr, type);
    }

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
		
		auto expr = make_expr(INT_LIT, typer->get("s32"));
        expr->int_value = tok->int_value;

        return expr;
	}

    if (tok->type == TOKEN_STRING_LIT) {
		lexer.eat_token();
		
		auto expr = make_expr(STRING_LIT, typer->make_pointer(typer->get("u8")));
        expr->string_lit = tok->lexeme;

        return expr;
    }

	if (tok->type == TOKEN_ATOM) {
		lexer.eat_token();

        if (eat_token_if('(')) {
            auto arguments = new std::vector<Expr *>();

            while (!eat_token_if(')')) {
                auto arg = parse_expr();
                arguments->push_back(arg);

                eat_token_if(',');
            }

            auto unmangled_name = tok->lexeme;
            auto func_decl = get_func(tok, arguments);

            auto return_types = func_decl->func_def.return_types;
			auto expr = make_expr(FUNCTION_CALL, (*return_types)[0]);
			expr->func_call.arguments = arguments;
            expr->func_call.target_func_decl = func_decl;

            return expr;
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
                    auto expr = make_expr(INT_LIT, typer->get("s32"));
                    expr->int_value = i;
                    return expr;
                }
            }

            messenger->report(category, "Value not found in enum");
        }

		auto var = scope->find(tok);
		auto expr = make_expr(VARIABLE, var->type);
		expr->var = var;

		return expr;
	}

	if (eat_token_if(TOKEN_NEW)) {
	    auto ty = parse_type();
	    auto expr = make_expr(NEW, typer->make_pointer(ty));
	    expr->alloc_type = ty;
	    return expr;
    }

	if (eat_token_if(TOKEN_TRUE)) {
	    auto expr = make_expr(INT_LIT, typer->get("bool"));
	    expr->int_value = 1;
	    return expr;
    }

	if (eat_token_if(TOKEN_FALSE)) {
	    auto expr = make_expr(INT_LIT, typer->get("bool"));
	    expr->int_value = 0;
	    return expr;
    }

	if (eat_token_if(TOKEN_NIL)) {
	    auto expr = make_expr(NIL, typer->make_pointer(typer->get("u8")));
	    return expr;
    }

	messenger->report_print_token(tok, "Unexpected token");
	return 0;
}

Expr *Parser::cast(Expr *target, QType *to) {
    auto expr = make_expr(CAST, to);
    expr->cast.from = target->type;
    expr->cast.to = to;
    expr->cast.target = target;

    return expr;
}

Expr *Parser::make_compare_zero(Expr *expr) {
    Expr *cmp_expr = make_expr(COMPARE_ZERO, typer->get("bool"));
    cmp_expr->target = expr;
    return cmp_expr;
}

QType *Parser::parse_type() {
	auto tok = lexer.peek_token();
	if (tok->type == '*') {
		lexer.eat_token();
		return typer->make_pointer(parse_type());
	}
	if (tok->type != TOKEN_ATOM) {
		messenger->report(tok, "Expected type");
	}
	lexer.eat_token();
	return typer->get(tok);
}

bool Parser::expr_is_targatable(Expr *expr) {
    return expr->kind == VARIABLE || expr->kind == DEREF || expr->kind == MEMBER;
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

const char *Parser::mangle_func(Stmt *stmt) {
    auto unmangled_name = stmt->func_def.unmangled_name;
	if (strcmp(unmangled_name, "main") == 0) {
		return unmangled_name;
	}
	std::stringstream ss;
	ss << unmangled_name;
	for (auto par : *stmt->func_def.parameters) {
		ss << "_" << mangle_type(par->type);
	}

    if (stmt->func_def.isvararg)
        ss << "@@@";

	std::string str = ss.str();
	char *mangled = new char[str.length() + 1];
	std::strcpy(mangled, str.c_str());
	return mangled;
}

std::string Parser::mangle_type(QType *type) {
	if (type->isuint()) {
		return "u";
	}
	if (type->isint()) {
		return "i";
	}
	if (type->isbool()) {
		return "b";
	}
	if (type->ispointer()) {
		return "p" + mangle_type(type->element_type);
	}
	return "";
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

void Parser::insert_func(Token *name_token, const char *mangled_name, Stmt *func_decl) {
    auto sname = std::string(mangled_name);
    auto it = functions.find(sname);
    if (it != functions.end()) {
        messenger->report(name_token, "Function already exists");
    }
    
    functions.insert(std::make_pair(sname, func_decl));
}

Stmt *Parser::get_func(Token *name_token, std::vector<Expr *> *arguments) {
    auto unmangled_name = name_token->lexeme;

    for (auto key : functions) {
        auto decl = key.second;
        if (strcmp(decl->func_def.unmangled_name, unmangled_name) != 0)
            continue;

        auto decl_args = decl->func_def.parameters;
        int decl_ac = decl_args->size();
        int call_ac = arguments->size();
        bool vararg = decl->func_def.isvararg;

        if (call_ac < decl_ac || (call_ac > decl_ac && !vararg))
            continue;

        bool matching = true;
        for (int i = 0; i < decl_ac; ++i) {
            auto arg = (*arguments)[i];
            QType *arg_type = arg->type;
            QType *par_type = (*decl_args)[i]->type;

            if (!typer->compare(arg_type, par_type)) {
                if (typer->can_convert(arg_type, par_type)) {
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

Stmt *Parser::make_stmt(StmtKind kind) {
    auto stmt = create_stmt();
    stmt->kind = kind;
    return stmt;
}

Expr *Parser::make_expr(ExprKind kind, QType *type) {
    auto expr = create_expr();
    expr->kind = kind;
    expr->type = type;
    return expr;
}

Variable *Parser::make_variable(const char *name, QType *type) {
    auto var = create_variable();
	var->name = name;
	var->type = type;
    return var;
}

Variable *Parser::add_or_get_variable(Token *token, QType *type) {
    Variable *var = scope->find_null(token->lexeme);
    if (var) {
        if (!typer->compare(var->type, type)) {
            messenger->report(token, "Variable already defined");
        }
        return var;
    }

    var = create_variable();
	var->name = token->lexeme;
	var->type = type;
	scope->add(token, var);
    return var;
}
