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
	{'+', 2},
	{'-', 2},
	{'*', 3},
	{'/', 3},
	{'%', 3},
};

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
            } else {
                QType *type = parse_type();

                if (lexer.peek_token()->type != '=') {
                    messenger->report(lexer.peek_token(), "Expected '=' after variable type specifier");
                }

                lexer.eat_token();

                return parse_variable_definition_type(tok, VAR_GLOBAL, type);
            }
        } else if (pt == TOKEN_COLON_COLON) {
            lexer.eat_token();
            lexer.eat_token();

            return parse_func_def(tok);
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

	if (eat_token_if(TOKEN_RETURN)) {
		return parse_return();
	}

	return try_parse_atom();
}

Stmt *Parser::parse_func_def(Token *name) {
    scope_push();

	auto stmt = make_stmt(FUNCTION_DEFINITION);

    auto cname = name->lexeme;
    stmt->func_def.unmangled_name = cname;

    parse_function_parameters(stmt, true);
    auto mname = mangle_func(stmt);
    stmt->func_def.mangled_name = mname;

    if (eat_token_if('{')) {
        stmt->func_def.return_type = typer->get("void");
    } else {
        auto return_type = parse_type();
        stmt->func_def.return_type = return_type;

        if (!eat_token_if('{')) {
            messenger->report(lexer.peek_token(), "Expected { after function declaration");
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

    if (eat_token_if(';')) {
        stmt->func_def.return_type = typer->get("void");
    } else {
        auto return_type = parse_type();
        stmt->func_def.return_type = return_type;

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

Stmt *Parser::parse_variable_definition_type(Token *name_token, u8 flags, QType *type) {
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
	stmt->var_def.value = val_expr;
    stmt->var_def.flags = flags;

	scope->add(name_token, stmt->var_def.var);

	eat_semicolon();

    return val_type;
}

Stmt *Parser::parse_return() {
    Expr *val;
    if (eat_token_if(';')) {
        val = 0;
    } else {
        val = parse_expr();
    	eat_semicolon();
    }

	auto stmt = make_stmt(RETURN);
	stmt->return_value = val;

	return stmt;
}

Stmt *Parser::try_parse_atom() {
	auto atom = lexer.peek_token();

	if (atom->type == TOKEN_ATOM) {
        if (lexer.peek_token(1)->type == ':') {
            lexer.eat_token();
            lexer.eat_token();

            if (eat_token_if('=')) {
                return parse_variable_definition(atom, false);
            } else {
                QType *type = parse_type();

                if (lexer.peek_token()->type != '=') {
                    messenger->report(lexer.peek_token(), "Expected '=' after variable type specifier");
                }

                lexer.eat_token();

                return parse_variable_definition_type(atom, false, type);
            }
        } 
	}

    auto stmt = make_stmt(EXPR_STMT);
    stmt->target_expr = parse_expr();
    eat_semicolon();

    return stmt;
}

Expr *Parser::parse_expr(int prec) {
	return parse_assign_or_binary(prec);
}

Expr *Parser::parse_assign_or_binary(int prec) {
	auto lhs = parse_unary();

	while (true) {
		auto tok = lexer.peek_token();
		auto tok_type = tok->type;
		auto op_it = operator_precedence.find(tok_type);

		if (op_it == operator_precedence.end())
			break;
		if (op_it->second < prec)
			break;

		lexer.eat_token();

        if (tok_type == '=' || (tok_type >= TOKEN_ADD_EQ && tok_type <= TOKEN_MOD_EQ)) {
            auto value = parse_assign_or_binary(prec + 1);
            auto lhs_type = lhs->type;
            auto rhs_type = value->type;

            if (!expr_is_targatable(lhs)) {
                messenger->report(tok, "Can't assign value to this expression");
            }

            if (!typer->compare(lhs_type, rhs_type)) {
                if (typer->can_convert(rhs_type, lhs_type)) {
                    value = cast(value, lhs_type);
                } else {
                    messenger->report(tok, "Type of variable and type of value are do not match");
                }
            }

			auto expr = make_expr(ASSIGN, lhs_type);
			expr->assign.target = lhs;
			expr->assign.value = value;
			expr->assign.op = tok_type;

            lhs = expr;
        } else {
            auto rhs = parse_assign_or_binary(prec + 1);
            auto lhs_type = lhs->type;
            auto rhs_type = rhs->type;

             if (!typer->compare(lhs_type, rhs_type)) {
                if (typer->can_convert(rhs_type, lhs_type)) {
                    rhs = cast(rhs, lhs_type);
                } else {
                    messenger->report(tok, "Lhs and Rhs of binary expression are of different types");
                }
            }

			auto expr = make_expr(BINARY, lhs_type);
			expr->bin.lhs = lhs;
			expr->bin.rhs = rhs;
			expr->bin.op = op_it->first;

            lhs = expr;
        }
	}

	return lhs;
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
        expr->deref_target = target;
		
        return expr;
    } else if (eat_token_if('+')) {
        return parse_postfix();
    } else if (eat_token_if('!')) {
        auto target = parse_postfix();

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

			auto expr = make_expr(FUNCTION_CALL, func_decl->func_def.return_type);
			expr->func_call.arguments = arguments;
            expr->func_call.target_func_decl = func_decl;

            return expr;
        }

		auto var = scope->find(tok);
		auto expr = make_expr(VARIABLE, var->type);
		expr->var = var;

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
    return expr->kind == VARIABLE || expr->kind == DEREF;
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
	if (type->isint()) {
		return "i";
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
	auto sname = std::string(token->lexeme);
	auto it = variables.find(sname);
	if (it == variables.end()) {
		if (parent) {
			return parent->find(token);
		}
        messenger->report(token, "Undefined variable");
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