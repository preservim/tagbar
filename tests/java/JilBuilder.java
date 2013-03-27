// This file is part of the Java Compiler Kit (JKit)
//
// The Java Compiler Kit is free software; you can 
// redistribute it and/or modify it under the terms of the 
// GNU General Public License as published by the Free Software 
// Foundation; either version 2 of the License, or (at your 
// option) any later version.
//
// The Java Compiler Kit is distributed in the hope
// that it will be useful, but WITHOUT ANY WARRANTY; without 
// even the implied warranty of MERCHANTABILITY or FITNESS FOR 
// A PARTICULAR PURPOSE.  See the GNU General Public License 
// for more details.
//
// You should have received a copy of the GNU General Public 
// License along with the Java Compiler Kit; if not, 
// write to the Free Software Foundation, Inc., 59 Temple Place, 
// Suite 330, Boston, MA  02111-1307  USA
//
// (C) David James Pearce, 2009. 

package jkit.java.stages;

import java.util.*;

import jkit.compiler.FieldNotFoundException;
import static jkit.compiler.SyntaxError.*;
import static jkit.jil.util.Exprs.*;
import static jkit.jil.util.Types.*;
import jkit.compiler.ClassLoader;
import jkit.compiler.Clazz;
import jkit.compiler.SyntacticAttribute;
import jkit.java.io.JavaFile;
import jkit.java.tree.*;
import jkit.jil.tree.*;
import jkit.jil.tree.Type;
import jkit.jil.util.Types;
import jkit.util.Pair;
import jkit.util.Triple;

/**
 * <p>
 * The aim of this stage is to generate Jil code representing this class.
 * </p>
 * 
 * @author djp
 * 
 */
public class JilBuilder {
	private ClassLoader loader = null;
	private TypeSystem types = null;
	
	/**
	 * This class is used to annotate an invoke expression with the list of
	 * checked (and possibly unchecked) exceptions it has declared to throw, as
	 * well as the type of the method to be invoked. It's sole purpose is to
	 * prevent us from having to re-traverse the class heirarchy during code
	 * generation.
	 * 
	 * @author djp
	 * 
	 */
	public static class MethodInfo implements SyntacticAttribute {
		public final ArrayList<Type.Clazz> exceptions;
		public Type.Function type;

		public MethodInfo(List<Type.Clazz> e, Type.Function type) {
			exceptions = new ArrayList<Type.Clazz>(e);
			this.type = type;
		}
	}
	
	private static class Scope {}
	
	private static class LabelScope extends Scope {
		public String label;
		public LabelScope(String lab) {
			label = lab;
		}
	}
	
	private static class SwitchScope extends Scope {
		public String exitLab;
		public SwitchScope(String el) {			
			exitLab = el;
		}
	}
	
	private static class LoopScope extends SwitchScope {
		public String continueLab;		
		public LoopScope(String cl, String el) {
			super(el);
			continueLab = cl;			
		}
	}
	
	private static class ClassScope extends Scope {
		public Type.Clazz type;
		
		public ClassScope(Type.Clazz type) {
			this.type = type;
		}
	}
	
	private final Stack<Scope> scopes = new Stack<Scope>();
	
	public JilBuilder(ClassLoader loader, TypeSystem types) {
		this.loader = loader;
		this.types = types;
	}
	
	public void apply(JavaFile file) {
		// Now, traverse the declarations
		for(Decl d : file.declarations()) {
			doDeclaration(d, null);
		}		
	}
	
	protected void doDeclaration(Decl d, JilClass parent) {
		try {
			if(d instanceof Decl.JavaInterface) {
				doInterface((Decl.JavaInterface)d);
			} else if(d instanceof Decl.JavaEnum) {
				doEnum((Decl.JavaEnum)d);
			} else if(d instanceof Decl.JavaClass) {
				doClass((Decl.JavaClass)d);
			} else if(d instanceof Decl.JavaMethod) {				
				doMethod((Decl.JavaMethod)d, parent);
			} else if(d instanceof Decl.JavaField) {
				doField((Decl.JavaField)d, parent);
			} else if (d instanceof Decl.InitialiserBlock) {
				doInitialiserBlock((Decl.InitialiserBlock)d , parent);
			} else if (d instanceof Decl.StaticInitialiserBlock) {
				doStaticInitialiserBlock((Decl.StaticInitialiserBlock) d, parent);
			} else {
				syntax_error("internal failure (unknown declaration \"" + d
						+ "\" encountered)", d);
			}
		} catch(Exception ex) {
			internal_error(d,ex);
		}							
	}
	
	protected void doInterface(Decl.JavaInterface d) throws ClassNotFoundException {
		doClass(d);
	}
	
	protected void doEnum(Decl.JavaEnum en) throws ClassNotFoundException {
		doClass(en);		
	}
	
	protected void doClass(Decl.JavaClass c) throws ClassNotFoundException {
		Type.Clazz type = c.attribute(Type.Clazz.class);
		scopes.push(new ClassScope(type));		
		// We, need to update the skeleton so that any methods and fields
		// discovered below this are attributed to this class!			
		JilClass skeleton = (JilClass) loader.loadClass(type);

		// I do fields after everything else, so as to simplify the process
		// of adding field initialisers to constructors. This is because it
		// means I can be sure that the constructor has been otherwise
		// completely generated, so all I need is to add initialisers at
		// beginning, after super call (if there is one).
		ArrayList<Decl> fields = new ArrayList<Decl>();
		for(Decl d : c.declarations()) {
			if (!(d instanceof Decl.JavaField)
					&& !(d instanceof Decl.InitialiserBlock)
					&& !(d instanceof Decl.StaticInitialiserBlock)) {				
				doDeclaration(d, skeleton);
			} else {
				fields.add(d);
			}
		}		
		
		// Note, I iterate the field declarations in reverse order to ensure
		// that field initialisers are added to constructors in the right
		// order.
		for(int i=fields.size();i>0;--i) {
			Decl d = fields.get(i-1);
			doDeclaration(d, skeleton);				
		}										
		
		scopes.pop();
	}

	protected void doMethod(Decl.JavaMethod d, JilClass parent) {	
		Type.Function type = d.attribute(Type.Function.class);
		List<JilStmt> stmts = doStatement(d.body());		
		
		// simple hack here, for case when no return statement is provided.
		if (type.returnType() instanceof Type.Void
				&& (stmts.size() == 0 || !(stmts.get(stmts.size() - 1) instanceof JilStmt.Return))) {			
			stmts.add(new JilStmt.Return(null));
		}
		
		// First, off. If this is a constructor, then check whether there is an
		// explicit super constructor call or not.  If not, then add one.
		if (d instanceof Decl.JavaConstructor) {			
			if(findSuperCall(stmts) == -1) {							
				stmts.add(0, new JilExpr.SpecialInvoke(new JilExpr.Variable("super", parent
						.superClass()), "super", new ArrayList<JilExpr>(),
						new Type.Function(T_VOID), T_VOID));
			} 
		}				
		
		// Now, add this statement list to the jil method representing this java
		// method.				
		String name = d instanceof Decl.JavaConstructor ? parent.name() : d.name();		
		for (JilMethod m : parent.methods()) {			
			if (m.name().equals(name) && m.type().equals(type)) {				
				m.body().addAll(stmts);
			}
		}				
	}

	protected void doField(Decl.JavaField d, JilClass parent) {		
		Pair<JilExpr,List<JilStmt>> tmp = doExpression(d.initialiser());
		Type fieldT = d.type().attribute(Type.class);
		boolean isStatic = d.isStatic();			
		
		if(tmp != null) {
			if(d.isStatic()) {				
				if(!(d.isFinal() && d.isConstant())) {
					// This is a static field with an non-constant initialiser.
					// Therefore, we need to add it to the static initialiser.
					JilMethod staticInit = createStaticInitialiser(parent);
					JilExpr.Deref df = new JilExpr.Deref(new JilExpr.ClassVariable(
							parent.type()), d.name(), isStatic, fieldT, d
							.attributes());
					JilStmt.Assign ae = new JilStmt.Assign(df, tmp.first(), d
							.attributes());
					// add them at the beginning to get the right ordering.
					staticInit.body().add(0,ae);
					staticInit.body().addAll(0,tmp.second());					
				} 
			} else {
				// This is a non-static field with an initialiser. Therefore, we
				// need to add it to the beginning of all constructors. One issue is
				// that, if the first statement of a constructor is a super call
				// (which is should normally be), then we need to put the statements
				// after that.
				for(JilMethod m : parent.methods()) {
					if(m.name().equals(parent.name())) {
						List<JilStmt> body = m.body();
						JilExpr.Deref df = new JilExpr.Deref(new JilExpr.Variable("this",
								parent.type()), d.name(), isStatic, fieldT, d
								.attributes());
						JilStmt.Assign ae = new JilStmt.Assign(df, tmp.first(), d
								.attributes());
						int sc = findSuperCall(body)+1;						
						body.add(sc,ae);
						body.addAll(sc,tmp.second());						
					}
				}
			}
		}				
	}
	
	protected void doInitialiserBlock(Decl.InitialiserBlock d, JilClass parent) {
		ArrayList<JilStmt> stmts = new ArrayList<JilStmt>();
		
		for (Stmt s : d.statements()) {
			stmts.addAll(doStatement(s));
		}	
		
		// This is a non-static initialiser block. Therefore, we
		// need to add it to the beginning of all constructors. One issue is
		// that, if the first statement of a constructor is a super call
		// (which is should normally be), then we need to put the statements
		// after that.		
		for(JilMethod m : parent.methods()) {
			if(m.name().equals(parent.name())) {
				List<JilStmt> body = m.body();				
				body.addAll(findSuperCall(body)+1,stmts);				
			}
		}		
	}
	
	protected void doStaticInitialiserBlock(Decl.StaticInitialiserBlock d, JilClass parent) {		
		ArrayList<JilStmt> stmts = new ArrayList<JilStmt>();
		for (Stmt s : d.statements()) {
			stmts.addAll(doStatement(s));
		}		
		
		JilMethod m = createStaticInitialiser(parent);
		// Again, add at beginning to ensure the right order.
		m.body().addAll(0,stmts);				
	}
	
	protected List<JilStmt> doStatement(Stmt e) {
		try {
			if(e instanceof Stmt.SynchronisedBlock) {
				return doSynchronisedBlock((Stmt.SynchronisedBlock)e);
			} else if(e instanceof Stmt.TryCatchBlock) {
				return doTryCatchBlock((Stmt.TryCatchBlock)e);
			} else if(e instanceof Stmt.Block) {
				return doBlock((Stmt.Block)e);
			} else if(e instanceof Stmt.VarDef) {
				return doVarDef((Stmt.VarDef) e);
			} else if(e instanceof Stmt.AssignmentOp) {
				return doAssignmentOp((Stmt.AssignmentOp) e).second();
			} else if(e instanceof Stmt.Assignment) {
				return doAssignment((Stmt.Assignment) e).second();
			} else if(e instanceof Stmt.Return) {
				return doReturn((Stmt.Return) e);
			} else if(e instanceof Stmt.Throw) {
				return doThrow((Stmt.Throw) e);
			} else if(e instanceof Stmt.Assert) {
				return doAssert((Stmt.Assert) e);
			} else if(e instanceof Stmt.Break) {
				return doBreak((Stmt.Break) e);
			} else if(e instanceof Stmt.Continue) {
				return doContinue((Stmt.Continue) e);
			} else if(e instanceof Stmt.Label) {
				return doLabel((Stmt.Label) e);
			} else if(e instanceof Stmt.If) {
				return doIf((Stmt.If) e);
			} else if(e instanceof Stmt.For) {
				return doFor((Stmt.For) e);
			} else if(e instanceof Stmt.ForEach) {
				return doForEach((Stmt.ForEach) e);
			} else if(e instanceof Stmt.While) {
				return doWhile((Stmt.While) e);
			} else if(e instanceof Stmt.DoWhile) {
				return doDoWhile((Stmt.DoWhile) e);
			} else if(e instanceof Stmt.Switch) {
				return doSwitch((Stmt.Switch) e);
			} else if(e instanceof Expr.Invoke) {
				Pair<JilExpr, List<JilStmt>> r = doInvoke((Expr.Invoke) e);
				r.second().add((JilExpr.Invoke) r.first());
				return r.second();
			} else if(e instanceof Expr.New) {
				Pair<JilExpr, List<JilStmt>> r =  doNew((Expr.New) e);
				r.second().add((JilExpr.New) r.first());
				return r.second();
			} else if(e instanceof Decl.JavaClass) {
				doClass((Decl.JavaClass)e);
				return new ArrayList<JilStmt>();
			} else if(e instanceof Stmt.PrePostIncDec) {
				Pair<JilExpr, List<JilStmt>> r = doExpression((Stmt.PrePostIncDec) e);
				return r.second();
			}
		} catch(Exception ex) {
			internal_error(e,ex);
		}
			
		if (e != null) {
			syntax_error("Invalid statement encountered: " + e.getClass(), e);
		}	
		
		return new ArrayList<JilStmt>();
	}
	
	protected List<JilStmt> doBlock(Stmt.Block block) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		if(block != null) {		
			// now process every statement in this block.
			for(Stmt s : block.statements()) {
				r.addAll(doStatement(s));
			}		
		}
		return r;
	}
	
	protected List<JilStmt> doCatchBlock(Stmt.CatchBlock block) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		if(block != null) {			
			// now process every statement in this block.
			for(Stmt s : block.statements()) {
				r.addAll(doStatement(s));
			}		
		}
		return r;
	}
	
	protected List<JilStmt> doSynchronisedBlock(Stmt.SynchronisedBlock block) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		r.addAll(doBlock(block));
		doExpression(block.expr());
		// need to add synch enter and leave here ?
		return r;
	}
	
	protected int tryexit_label = 0;
	protected int tryhandler_label = 0;
	
	protected List<JilStmt> doTryCatchBlock(Stmt.TryCatchBlock block) {
		String exitLab = "tryexit" + tryexit_label++;
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		r.addAll(doBlock(block));
		
		int i = tryhandler_label;
		for(Stmt.CatchBlock cb : block.handlers()) {
			String handlerLab = "tryhandler" + i++;
			for(int j=0;j!=r.size();++j) {
				JilStmt s = r.get(j);
				Type.Clazz ct = cb.type().attribute(Type.Clazz.class);
				if(!(s instanceof JilStmt.Goto || s instanceof JilStmt.Label)) {
					// This is very pedantic. Almost certainly, we could rule out
					// certain kinds of exception branches. However, it's difficult
					// to do this properly and, therefore, we remain quite
					// conservative at this point. See the following paper for an
					// excellent discussion of this:
					// 
					// "Improving the precision and correctness of exception
					// analysis in Soot", John Jorgensen, 2003. 
					r.set(j,s.addException(ct, handlerLab));
				}
			}
		}
		
		r.add(new JilStmt.Goto(exitLab,block.attributes()));

		for(Stmt.CatchBlock cb : block.handlers()) {
			String handlerLab = "tryhandler" + tryhandler_label++;
			Type.Clazz ct = cb.type().attribute(Type.Clazz.class);
			r.add(new JilStmt.Label(handlerLab,cb.attributes()));
			r.add(new JilStmt.Assign(new JilExpr.Variable(cb.variable(), ct, cb
					.attributes()), new JilExpr.Variable("$", ct,
					cb.attributes())));					
			// At this point, we have a slightly interesting problem. We need to
			// search			
			r.addAll(doCatchBlock(cb));
			r.add(new JilStmt.Goto(exitLab,cb.attributes()));
		}
		
		r.add(new JilStmt.Label(exitLab,block.attributes()));
				
		List<JilStmt> finallyBlock = doBlock(block.finaly());		
		if(!finallyBlock.isEmpty()) {
			// Now, we add the finally block. This is done in a separate method
			// because it's actually quite challenging.
			addFinallyBlock(r,finallyBlock);			
		}
		
		return r;
	}
	
	protected static int finallyex_label = 0;
	
	protected void addFinallyBlock(List<JilStmt> block, List<JilStmt> finallyBlk) {
		// So, to add the finally block properly, we need to iterate through the
		// block and find any situations where we exit the block. This includes
		// return statements, branches to labels which are not in the block,
		// exceptional flow, and simply executing the last statement of the
		// block.
		
		// First, collect all local labels, so we can distinguish local from
		// non-local branches.
		HashSet<String> labels = new HashSet<String>();
		for(JilStmt s : block) {
			if(s instanceof JilStmt.Label) {
				JilStmt.Label l = (JilStmt.Label) s;
				labels.add(l.label());				
			}
		}			
		
		String exceptionLabel = "finally" + finallyex_label++; 
		
		// Now, iterate the block looking for non-local branches.
		boolean lastNonBranch = false;
		for(int i=0;i!=block.size();++i) {
			lastNonBranch = true;
			JilStmt stmt = block.get(i);
			if(stmt instanceof JilStmt.Return) {
				block.addAll(i,copyBlock(finallyBlk));
				i += finallyBlk.size();
				lastNonBranch = false;
			} else if(stmt instanceof JilStmt.Goto) {
				JilStmt.Goto g = (JilStmt.Goto) stmt;
				if(!labels.contains(g.label())) {
					// this is a non-local branch statement.
					block.addAll(i,copyBlock(finallyBlk));
					i += finallyBlk.size();					
				} 
				lastNonBranch = false;
			} else if(stmt instanceof JilStmt.IfGoto) {
				JilStmt.IfGoto g = (JilStmt.IfGoto) stmt;
				
				if(!labels.contains(g.label())) {
					// houston, we have a problem. I'm not really sure how we
					// can actually get here though.
					throw new RuntimeException(
							"An internal failure has occurred in JilBuilder.  Please report it, along with the generating code!");
				}				
			} else if(!(stmt instanceof JilStmt.Label)){				
				// Add the default exceptional edge for exceptional flow.				
				stmt = stmt.addException(Types.JAVA_LANG_THROWABLE, exceptionLabel);
				block.set(i, stmt);
				
				// just for the (unlikely) case when last statement is a throw.
				if(stmt instanceof JilStmt.Throw) {	lastNonBranch = false; } 
			}
		}
		
		String exitLabel = "finallyexit" + (finallyex_label-1); 
		if(lastNonBranch) {
			block.addAll(finallyBlk);
			block.add(new JilStmt.Goto(exitLabel));
		}		
		
		// Now, process exceptional exit
		block.add(new JilStmt.Label(exceptionLabel));
		block.add(new JilStmt.Assign(new JilExpr.Variable(exceptionLabel + "$",
				Types.JAVA_LANG_THROWABLE), new JilExpr.Variable("$",
				Types.JAVA_LANG_THROWABLE)));	
		block.addAll(copyBlock(finallyBlk));
		block.add(new JilStmt.Throw(new JilExpr.Variable(exceptionLabel + "$",
				Types.JAVA_LANG_THROWABLE)));
		
		if(lastNonBranch) {
			block.add(new JilStmt.Label(exitLabel));
		}
	}
	
	protected static int copy_label = 0;
	protected List<JilStmt> copyBlock(List<JilStmt> block) {
		// The purpose of this method is to create a copy of the block.
		// In particular, labels within the block must be copied.
				
		HashSet<String> labels = new HashSet<String>();
		for(JilStmt stmt : block) {
			if(stmt instanceof JilStmt.Label) {
				JilStmt.Label lab = (JilStmt.Label) stmt;
				labels.add(lab.label());
			}
		}
		
		ArrayList<JilStmt> nblock = new ArrayList<JilStmt>();
		for(JilStmt stmt : block) {
			ArrayList<Pair<Type.Clazz, String>> nexceptions = new ArrayList();
			
			for(Pair<Type.Clazz, String> p : stmt.exceptions()) {
				String target = p.second();
				if(labels.contains(target)) {
					target = target + "$copy" + copy_label;
				} 
				nexceptions.add(new Pair(p.first(),target));
			}
			
			if(stmt instanceof JilStmt.Goto) {
				JilStmt.Goto gto = (JilStmt.Goto) stmt;
				String target = gto.label();
				if(labels.contains(target)) {
					target = target + "$copy" + copy_label;
				} 
				nblock.add(new JilStmt.Goto(target, nexceptions,
						new ArrayList<SyntacticAttribute>(stmt.attributes())));
			} else if(stmt instanceof JilStmt.IfGoto) {
				JilStmt.IfGoto igto = (JilStmt.IfGoto) stmt;
				String target = igto.label();
				if(labels.contains(target)) {
					target = target + "$copy" + copy_label;
				} 
				nblock.add(new JilStmt.IfGoto(igto.condition(), target,
						nexceptions,
						new ArrayList<SyntacticAttribute>(stmt.attributes())));
			} else if(stmt instanceof JilStmt.Switch) {
				JilStmt.Switch swt = (JilStmt.Switch) stmt;
				ArrayList<Pair<JilExpr.Number,String>> ncases = new ArrayList();
				for(Pair<JilExpr.Number,String> c : swt.cases()) {
					String target = c.second();				
					if(labels.contains(target)) {
						target = target + "$copy" + copy_label;
					} 	
					ncases.add(new Pair(c.first(),target));
				}
				// And, don't forget the default label!
				String deftarget = swt.defaultLabel();
				if(labels.contains(deftarget)) {
					deftarget = deftarget + "$copy" + copy_label;
				} 	
				nblock.add(new JilStmt.Switch(swt.condition(), ncases,
						deftarget, new ArrayList<SyntacticAttribute>(swt.attributes())));
			} else if(stmt instanceof JilStmt.Label) {			
				JilStmt.Label lab = (JilStmt.Label) stmt;
				String target = lab.label();
				if(labels.contains(target)) {
					target = target + "$copy" + copy_label;
				} 
				nblock.add(new JilStmt.Label(target,
						new ArrayList<SyntacticAttribute>(stmt.attributes())));
			} else {
				// there is a bug relating to switch statements.
				JilStmt nstmt = stmt.clearAddExceptions(nexceptions);
				nblock.add(nstmt);
			}
		}
		
		copy_label = copy_label + 1;
		
		return nblock;
	}
	
	protected List<JilStmt> doVarDef(Stmt.VarDef def) {		
		Type type = def.type().attribute(Type.class);
		List<Triple<String, Integer, Expr>> defs = def.definitions();
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		for(int i=0;i!=defs.size();++i) {
			Triple<String, Integer, Expr> d = defs.get(i);
			Type nt = type;
													
			for(int j=0;j!=d.second();++j) {
				nt = new Type.Array(nt);
			}

			if(d.third() != null) {
				Pair<JilExpr,List<JilStmt>> e = doExpression(d.third());
				r.addAll(e.second());
				JilExpr lhs = new JilExpr.Variable(d.first(), nt, def
						.attributes());
				r.add(new JilStmt.Assign(lhs, e.first()));
			}
		}
		
		return r;
	}		
	
	protected Pair<JilExpr,List<JilStmt>> doAssignmentOp(Stmt.AssignmentOp def) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		Pair<JilExpr,List<JilStmt>> lhs = doExpression(def.lhs());
		Pair<JilExpr, List<JilStmt>> rhs = doExpression(new Expr.BinOp(
				def.op(), def.lhs(), def.rhs(), def.attributes()));
		
		Type lhs_t = lhs.first().type();

		if (rhs.second().isEmpty() || lhs.first() instanceof JilExpr.Variable) {		
			JilExpr tmpVar = new JilExpr.Variable(getTempVar(), lhs_t, def
					.attributes());
			r.add(new JilStmt.Assign(tmpVar, lhs.first(), def.attributes()));
			r.addAll(rhs.second());
			r.add(new JilStmt.Assign(lhs.first(), rhs.first(), def
					.attributes()));
			return new Pair(lhs.first(), r);
		} else if(lhs.first() instanceof JilExpr.Deref) {			
			JilExpr.Deref deref1 = (JilExpr.Deref) lhs.first(); 

			if(deref1.target() instanceof JilExpr.ClassVariable) {
				// Slightly awkward case, since this corresponds to a static
                // class access and, hence, there are no possible side-effects.
                // However, we cannot assign the target to a tmp variable, since
                // it will not compile down to anything in practice.
				r.addAll(rhs.second());
				r.add(new JilStmt.Assign(lhs.first(), rhs.first(), def
								.attributes()));
				return new Pair(lhs.first(), r);
			} else {

				JilExpr tmpVar = new JilExpr.Variable(getTempVar(), deref1.target()
						.type(), def.attributes());
				r.add(new JilStmt.Assign(tmpVar, deref1.target(), def.attributes()));
				r.addAll(rhs.second());
				JilExpr.Deref deref2 = new JilExpr.Deref(tmpVar, deref1.name(),
						deref1.isStatic(), deref1.type(), deref1.attributes()); 
				r
				.add(new JilStmt.Assign(deref2, rhs.first(), def
						.attributes()));
				return new Pair(deref2, r);
			}
		} else if(lhs.first() instanceof JilExpr.ArrayIndex) {
			JilExpr.ArrayIndex aindex1 = (JilExpr.ArrayIndex) lhs.first();

			JilExpr targetVar = new JilExpr.Variable(getTempVar(), aindex1
					.target().type(), def.attributes());
			JilExpr indexVar = new JilExpr.Variable(getTempVar(), aindex1
					.index().type(), def.attributes());
			r.add(new JilStmt.Assign(targetVar, aindex1.target(), def
					.attributes()));
			r.add(new JilStmt.Assign(indexVar, aindex1.index(), def
					.attributes()));
			r.addAll(rhs.second());
			JilExpr.ArrayIndex aindex2 = new JilExpr.ArrayIndex(targetVar,
					indexVar, aindex1.type(), aindex1.attributes());
			r
			.add(new JilStmt.Assign(aindex2, rhs.first(), def
					.attributes()));
			return new Pair(aindex2, r);
		} else {
			syntax_error(
					"unknown l-value encountered on assignment with side-effects",
					def);
			return null; // unreachable.
		}	
	}
	
	protected Pair<JilExpr,List<JilStmt>> doAssignment(Stmt.Assignment def) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		Pair<JilExpr,List<JilStmt>> lhs = doExpression(def.lhs());	
		Pair<JilExpr,List<JilStmt>> rhs = doExpression(def.rhs());
		r.addAll(lhs.second());
		
		if (rhs.second().isEmpty() || lhs.first() instanceof JilExpr.Variable) {
			// easy case, no side-effects in rhs or trivial assignment.
			r.addAll(rhs.second());
			r.add(new JilStmt.Assign(lhs.first(), rhs.first(), def
							.attributes()));
			return new Pair(lhs.first(), r);
		} else if(lhs.first() instanceof JilExpr.Deref) {
			
			JilExpr.Deref deref1 = (JilExpr.Deref) lhs.first(); 

			if(deref1.target() instanceof JilExpr.ClassVariable) {
				// Slightly awkward case, since this corresponds to a static
                // class access and, hence, there are no possible side-effects.
                // However, we cannot assign the target to a tmp variable, since
                // it will not compile down to anything in practice.
				r.addAll(rhs.second());
				r.add(new JilStmt.Assign(lhs.first(), rhs.first(), def
								.attributes()));
				return new Pair(lhs.first(), r);
			} else {
				JilExpr tmpVar = new JilExpr.Variable(getTempVar(), deref1.target()
						.type(), def.attributes());
				r.add(new JilStmt.Assign(tmpVar, deref1.target(), def.attributes()));
				r.addAll(rhs.second());
				JilExpr.Deref deref2 = new JilExpr.Deref(tmpVar, deref1.name(),
						deref1.isStatic(), deref1.type(), deref1.attributes()); 
				r.add(new JilStmt.Assign(deref2, rhs.first(), def
						.attributes()));
				return new Pair(deref2, r);
			}			
		} else if(lhs.first() instanceof JilExpr.ArrayIndex) {
			JilExpr.ArrayIndex aindex1 = (JilExpr.ArrayIndex) lhs.first();

			JilExpr targetVar = new JilExpr.Variable(getTempVar(), aindex1
					.target().type(), def.attributes());
			JilExpr indexVar = new JilExpr.Variable(getTempVar(), aindex1
					.index().type(), def.attributes());
			r.add(new JilStmt.Assign(targetVar, aindex1.target(), def
					.attributes()));
			r.add(new JilStmt.Assign(indexVar, aindex1.index(), def
					.attributes()));
			r.addAll(rhs.second());
			JilExpr.ArrayIndex aindex2 = new JilExpr.ArrayIndex(targetVar,
					indexVar, aindex1.type(), aindex1.attributes());
			r.add(new JilStmt.Assign(aindex2, rhs.first(), def.attributes()));
			return new Pair(aindex2, r);
		} else {
			syntax_error(
					"unknown l-value encountered on assignment with side-effects",
					def);
			return null; // unreachable.
		}
	}
	
	protected List<JilStmt> doReturn(Stmt.Return ret) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		if(ret.expr() != null) {
			Pair<JilExpr,List<JilStmt>> expr = doExpression(ret.expr());
			r.addAll(expr.second());
			r.add(new JilStmt.Return(expr.first(),ret.attributes()));
		} else {
			r.add(new JilStmt.Return(null,ret.attributes()));
		}
		return r;
	}
	
	protected List<JilStmt> doThrow(Stmt.Throw ret) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		Pair<JilExpr,List<JilStmt>> expr = doExpression(ret.expr());
		r.addAll(expr.second());
		r.add(new JilStmt.Throw(expr.first(),ret.attributes()));
		return r;
	}
	
	protected List<JilStmt> doAssert(Stmt.Assert ret) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		Pair<JilExpr,List<JilStmt>> expr = doExpression(ret.expr());
		
		// need to do some real code generation here.
		
		return r;
	}
	
	protected List<JilStmt> doBreak(Stmt.Break brk) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		if(brk.label() == null) {
			SwitchScope ls = (SwitchScope) findEnclosingScope(SwitchScope.class);
			if(ls == null) {
				syntax_error("break used outside of loop or switch",brk);
			}
			r.add(new JilStmt.Goto(ls.exitLab,brk.attributes()));
		} else {
			String label = brk.label();
			LoopScope lastLoop = null;
			for(int i=scopes.size()-1;i>=0;--i) {
				Scope s = scopes.get(i);
				if(s instanceof LoopScope) {
					lastLoop = (LoopScope) s;
				} else if(s instanceof LabelScope) {
					LabelScope lab = (LabelScope) s;
					if(lab.label.equals(label)) {
						break;
					}
				}
			}
			if(lastLoop == null) {
				syntax_error("no enclosing loop instance",brk);
			}
			r.add(new JilStmt.Goto(lastLoop.exitLab,brk.attributes()));
		}
		
		return r;
	}
	
	protected List<JilStmt> doContinue(Stmt.Continue brk) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		if(brk.label() == null) {
			LoopScope ls = (LoopScope) findEnclosingScope(LoopScope.class);
			
			if(ls == null) {
				syntax_error("continue used outside loop",brk);				
			}
			
			r.add(new JilStmt.Goto(ls.continueLab,brk.attributes()));
		} else {
			String label = brk.label();
			LoopScope lastLoop = null;
			for(int i=scopes.size()-1;i>=0;--i) {
				Scope s = scopes.get(i);
				if(s instanceof LoopScope) {
					lastLoop = (LoopScope) s;
				} else if(s instanceof LabelScope) {
					LabelScope lab = (LabelScope) s;
					if(lab.label.equals(label)) {
						break;
					}
				}
			}
			if(lastLoop == null) {
				syntax_error("no enclosing loop instance",brk);
			}
			r.add(new JilStmt.Goto(lastLoop.continueLab,brk.attributes()));
		}
		
		return r;
	}
	
	protected List<JilStmt> doLabel(Stmt.Label lab) {
		scopes.push(new LabelScope(lab.label()));
		List<JilStmt> r = doStatement(lab.statement());
		scopes.pop();
		r.add(0, new JilStmt.Label(lab.label(), lab.attributes()));
		return r;
	}
	
	static protected int ifexit_label = 0;
	static protected int iftrue_label = 0;
	
	protected List<JilStmt> doIf(Stmt.If stmt) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		Pair<JilExpr,List<JilStmt>> cond = doExpression(stmt.condition());
		List<JilStmt> tbranch = doStatement(stmt.trueStatement());
		List<JilStmt> fbranch = doStatement(stmt.falseStatement());
		
		r.addAll(cond.second());
		
		if(stmt.falseStatement() == null) {
			r.add(new JilStmt.IfGoto(
					new JilExpr.UnOp(cond.first(), JilExpr.UnOp.NOT, T_BOOL,
					stmt.condition().attributes()), "ifexit" + ifexit_label, stmt.attributes()));
			r.addAll(tbranch);
		} else if(stmt.trueStatement() == null) {
			r.add(new JilStmt.IfGoto(cond.first(),"ifexit" + ifexit_label,stmt.attributes()));
			r.addAll(fbranch);
		} else {
			r.add(new JilStmt.IfGoto(cond.first(),"iftrue" + iftrue_label,stmt.attributes()));
			r.addAll(fbranch);
			r.add(new JilStmt.Goto("ifexit" + ifexit_label,stmt.attributes()));
			r.add(new JilStmt.Label("iftrue" + iftrue_label++,stmt.attributes()));
			r.addAll(tbranch);
		}
		
		r.add(new JilStmt.Label("ifexit" + ifexit_label++,stmt.attributes()));
		return r;
		
	}
	
	static protected int whileheader_label = 0;
	static protected int whileexit_label = 0;
	
	protected List<JilStmt> doWhile(Stmt.While stmt) {
		String headerLab = "whileheader" + whileheader_label++;
		String exitLab = "whileexit" + whileexit_label++;
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		r.add(new JilStmt.Label(headerLab, stmt
				.attributes()));
		Pair<JilExpr, List<JilStmt>> cond = doExpression(stmt.condition());
		r.addAll(cond.second());
		r.add(new JilStmt.IfGoto(new JilExpr.UnOp(cond.first(), JilExpr.UnOp.NOT,
				T_BOOL, stmt.condition().attributes()), exitLab, stmt
				.attributes()));
		scopes.push(new LoopScope(headerLab,exitLab));
		r.addAll(doStatement(stmt.body()));
		scopes.pop();
		r.add(new JilStmt.Goto(headerLab, stmt
				.attributes()));
		r.add(new JilStmt.Label(exitLab, stmt
						.attributes()));
		
		return r;
	}
	
	static protected int dowhileheader_label = 0;	
	static protected int dowhileexit_label = 0;
	
	protected List<JilStmt> doDoWhile(Stmt.DoWhile stmt) {
		String headerLab = "dowhileheader" + dowhileheader_label++;
		String exitLab = "dowhileexit" + dowhileexit_label++;
		
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		r.add(new JilStmt.Label(headerLab, stmt
				.attributes()));
		scopes.push(new LoopScope(headerLab,exitLab));
		r.addAll(doStatement(stmt.body()));
		scopes.pop();
		Pair<JilExpr, List<JilStmt>> cond = doExpression(stmt.condition());
		r.addAll(cond.second());
		r.add(new JilStmt.IfGoto(cond.first(), headerLab, stmt.attributes()));
		r.add(new JilStmt.Label(exitLab, stmt
				.attributes()));				
		return r;		
	}
	
	static protected int forheader_label = 0;
	static protected int forinc_label = 0;
	static protected int forexit_label = 0;
	
	protected List<JilStmt> doFor(Stmt.For stmt) {
		String headerLab = "forheader" + forheader_label++;
		String exitLab = "forexit" + forexit_label++;
		String incLab = "forinc" + forinc_label++;
		
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		if(stmt.initialiser() != null) {			
			r.addAll(doStatement(stmt.initialiser()));
		}
		
		r.add(new JilStmt.Label(headerLab, stmt
				.attributes()));
		
		if(stmt.condition() != null) {
			Pair<JilExpr, List<JilStmt>> cond = doExpression(stmt.condition());
			r.addAll(cond.second());
			r.add(new JilStmt.IfGoto(new JilExpr.UnOp(cond.first(), JilExpr.UnOp.NOT,
					T_BOOL, stmt.condition().attributes()), exitLab,
					stmt.attributes()));
		}
		
		if(stmt.increment() != null) {
			scopes.push(new LoopScope(incLab,exitLab));
		} else {
			// this is a minor optimisation in the case that no increment is
			// provided.
			scopes.push(new LoopScope(headerLab,exitLab));
		}
		r.addAll(doStatement(stmt.body()));
		scopes.pop();
				
		if(stmt.increment() != null) {
			r.add(new JilStmt.Label(incLab));			
			r.addAll(doStatement(stmt.increment()));
		}
		
		r.add(new JilStmt.Goto(headerLab, stmt
				.attributes()));
		r.add(new JilStmt.Label(exitLab, stmt
				.attributes()));
		
		return r;
	}
	
	static protected int forallheader_label = 0;
	static protected int forallexit_label = 0;
	static protected int foralliter_label = 0;
	static protected int forallinc_label = 0;
	
	protected List<JilStmt> doForEach(Stmt.ForEach stmt) {
		String headerLab = "forallheader" + forallheader_label++;
		String exitLab = "forallexit" + forallexit_label++;
		String iterLab = "foralliter" + foralliter_label++;
		String incLab = "forallinc" + forallinc_label++;
		
		ArrayList<JilStmt> stmts = new ArrayList<JilStmt>();
		
		Pair<JilExpr,List<JilStmt>> src = doExpression(stmt.source());
		JilExpr.Variable loopVar = new JilExpr.Variable(stmt.var(), stmt.type()
				.attribute(Type.class), stmt.attributes());
		
		Type srcType = src.first().type();				
		
		stmts.addAll(src.second());
		JilExpr.Variable iter;
		
		if (srcType instanceof Type.Array) {
			iter = new JilExpr.Variable(iterLab, T_INT);
			stmts
					.add(new JilStmt.Assign(iter, new JilExpr.Int(0), stmt
							.attributes()));
		} else {
			// the following needs to be expanded upon, so as to include generic
			// information on the iterator. The easiest way to do this is to
			// look up the iterator() method in the src class, and use it's
			// return type.
			iter = new JilExpr.Variable(iterLab, JAVA_UTIL_ITERATOR);			 

			stmts.add(new JilStmt.Assign(iter, new JilExpr.Invoke(src.first(),
					"iterator", new ArrayList<JilExpr>(), new Type.Function(
							JAVA_UTIL_ITERATOR), JAVA_UTIL_ITERATOR), stmt
					.attributes()));
		}				
		
		stmts.add(new JilStmt.Label(headerLab, stmt
				.attributes()));
		
		// Second, do condition
		
		if (srcType instanceof Type.Array) {
			Type.Array arrType = (Type.Array) srcType;
			JilExpr arrlength = new JilExpr.Deref(src.first(),"length",false,T_INT, stmt
					.attributes());
			JilExpr gecmp = new JilExpr.BinOp(iter,arrlength,JilExpr.BinOp.GTEQ,T_BOOL, stmt
					.attributes());
			stmts.add(new JilStmt.IfGoto(gecmp,exitLab, stmt
					.attributes()));					
			
			stmts.add(new JilStmt.Assign(loopVar, implicitCast(new JilExpr.ArrayIndex(src.first(),
					iter, arrType.element()),loopVar.type())));
		} else {
			JilExpr hasnext = new JilExpr.Invoke(iter, "hasNext",
					new ArrayList<JilExpr>(), new Type.Function(T_BOOL),
					T_BOOL, stmt.attributes());
			stmts.add(new JilStmt.IfGoto(new JilExpr.UnOp(hasnext, JilExpr.UnOp.NOT,
					T_BOOL), exitLab));
			
			JilExpr cast;
			if(loopVar.type() instanceof Type.Primitive) {
				// In this case, we have to deal with casting and implicit
				// conversion.
				JilExpr next = new JilExpr.Invoke(iter, "next", new ArrayList<JilExpr>(),
						new Type.Function(JAVA_LANG_OBJECT),
						boxedType((Type.Primitive) loopVar.type()), stmt.attributes());
				
				cast = implicitCast(next, loopVar.type());
			} else {
				JilExpr next = new JilExpr.Invoke(iter, "next", new ArrayList<JilExpr>(),
						new Type.Function(JAVA_LANG_OBJECT),
						loopVar.type(), stmt.attributes());
				cast = new JilExpr.Cast(next, loopVar.type());
			}
			stmts.add(new JilStmt.Assign(loopVar, cast, stmt.attributes()));			
		}
		
		// Third, do body
		
		if(srcType instanceof Type.Array) {
			scopes.push(new LoopScope(incLab,exitLab));
		} else {
			scopes.push(new LoopScope(headerLab,exitLab));
		}
		stmts.addAll(doStatement(stmt.body()));	
		scopes.pop();
		
		// Fourth, do increment
		if (srcType instanceof Type.Array) {
			stmts.add(new JilStmt.Label(incLab));
			forallinc_label++;
			JilExpr.BinOp rhs = new JilExpr.BinOp(iter, new JilExpr.Int(1),
					JilExpr.BinOp.ADD, T_INT, stmt.attributes());
			stmts.add(new JilStmt.Assign(iter,rhs,stmt.attributes()));
		} 
		
		stmts.add(new JilStmt.Goto(headerLab,stmt.attributes()));
		
		stmts.add(new JilStmt.Label(exitLab, stmt
				.attributes()));
		
		return stmts;
	}
	
	protected int switchcase_label = 0;
	protected int switchexit_label = 0;
	protected List<JilStmt> doSwitch(Stmt.Switch sw) {
		String switchExitLab = "switchexit" + switchexit_label++;
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		
		Pair<JilExpr,List<JilStmt>> cond = doExpression(sw.condition());
		ArrayList<Pair<JilExpr.Number,String>> cases = new ArrayList();
		ArrayList<JilStmt> caseStmts = new ArrayList();
		String defaultLab = null;
		for(Stmt.Case c : sw.cases()) {			
			Pair<JilExpr,List<JilStmt>> ce = doExpression(c.condition());
			String caseLab = "switchcase" + switchcase_label++;
			caseStmts.add(new JilStmt.Label(caseLab));
			scopes.push(new SwitchScope(switchExitLab));
			for(Stmt s : c.statements()) {				
				caseStmts.addAll(doStatement(s));				
			}			
			scopes.pop();
			if(c.condition() != null) {
				JilExpr ce_first = ce.first();				
				if(ce_first instanceof JilExpr.Number) {
					cases.add(new Pair(ce_first,caseLab));
				} else {					
					syntax_error("constant expression required",c.condition());
				}
			} else {
				defaultLab = caseLab;
			}			
		}
		if(defaultLab == null) { defaultLab = switchExitLab; }
		r.addAll(cond.second());
		r.add(new JilStmt.Switch(cond.first(),cases,defaultLab));
		r.addAll(caseStmts);
		r.add(new JilStmt.Label(switchExitLab));
		return r;
	}
	
	protected Pair<JilExpr,List<JilStmt>> doExpression(Expr e) {	
		try {
			if(e instanceof Value.Bool) {
				return doBoolVal((Value.Bool)e);
			} else if(e instanceof Value.Byte) {
				return doByteVal((Value.Byte)e);
			} else if(e instanceof Value.Char) {
				return doCharVal((Value.Char)e);
			} else if(e instanceof Value.Short) {
				return doShortVal((Value.Short)e);
			} else if(e instanceof Value.Int) {
				return doIntVal((Value.Int)e);
			} else if(e instanceof Value.Long) {
				return doLongVal((Value.Long)e);
			} else if(e instanceof Value.Float) {
				return doFloatVal((Value.Float)e);
			} else if(e instanceof Value.Double) {
				return doDoubleVal((Value.Double)e);
			} else if(e instanceof Value.String) {
				return doStringVal((Value.String)e);
			} else if(e instanceof Value.Null) {
				return doNullVal((Value.Null)e);
			} else if(e instanceof Value.TypedArray) {
				return doTypedArrayVal((Value.TypedArray)e);
			} else if(e instanceof Value.Array) {
				return doArrayVal((Value.Array)e);
			} else if(e instanceof Value.Class) {
				return doClassVal((Value.Class) e);
			} else if(e instanceof Expr.LocalVariable) {
				return doLocalVariable((Expr.LocalVariable)e);
			} else if(e instanceof Expr.NonLocalVariable) {
				return doNonLocalVariable((Expr.NonLocalVariable)e);
			} else if(e instanceof Expr.ClassVariable) {
				return doClassVariable((Expr.ClassVariable)e);
			} else if(e instanceof Expr.UnOp) {
				return doUnOp((Expr.UnOp)e);
			} else if(e instanceof Expr.BinOp) {
				return doBinOp((Expr.BinOp)e);
			} else if(e instanceof Expr.TernOp) {
				return doTernOp((Expr.TernOp)e);
			} else if(e instanceof Expr.Cast) {
				return doCast((Expr.Cast)e);
			} else if(e instanceof Expr.Convert) {
				return doConvert((Expr.Convert)e);
			} else if(e instanceof Expr.InstanceOf) {
				return doInstanceOf((Expr.InstanceOf)e);
			} else if(e instanceof Expr.Invoke) {
				return doInvoke((Expr.Invoke) e);
			} else if(e instanceof Expr.New) {
				return doNew((Expr.New) e);
			} else if(e instanceof Expr.ArrayIndex) {
				return doArrayIndex((Expr.ArrayIndex) e);
			} else if(e instanceof Expr.Deref) {
				return doDeref((Expr.Deref) e);
			} else if(e instanceof Stmt.AssignmentOp) {
				// force brackets			
				return doAssignmentOp((Stmt.AssignmentOp) e);			
			} else if(e instanceof Stmt.Assignment) {
				// force brackets			
				return doAssignment((Stmt.Assignment) e);			
			}
		} catch(Exception ex) {
			internal_error(e,ex);
		}
		
		if(e != null) {
			syntax_error("Invalid expression encountered: "
					+ e.getClass(),e);			
		}
		
		return null;
	}
	
	protected Pair<JilExpr, List<JilStmt>> doDeref(Expr.Deref e)
			throws ClassNotFoundException, FieldNotFoundException {
		Pair<JilExpr,List<JilStmt>> target = doExpression(e.target());
		Type type = e.attribute(Type.class);
		Type.Reference _targetT = e.target().attribute(Type.Reference.class);
		
		if(_targetT instanceof Type.Clazz) {
			Type.Clazz targetT = (Type.Clazz) _targetT;
			if(e.name().equals("this")) {
				// This is a special case, where we're trying to look up a field
				// called "this". No such field can exist! What this means is that
				// we're inside an inner class, and we're trying to access the this
				// pointer of an enclosing class. 
				ClassScope cs = (ClassScope) findEnclosingScope(ClassScope.class);
				int level =  cs.type.components().size() - targetT.components().size();
				JilExpr r = new JilExpr.Variable("this",cs.type);
				Type.Clazz t = cs.type;
				while(level > 0) {
					t = parentType(t);
					r = new JilExpr.Deref(r,"this$0",false,t);
					level = level - 1;
				}
				return new Pair(r,  new ArrayList<JilStmt>());
			} else {								
				Triple<Clazz, Clazz.Field, Type> r = types
				.resolveField(targetT, e.name(), loader);

				return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Deref(target.first(), e
						.name(),r.second().isStatic(), type,  e.attributes()),
						target.second());				
			}
		} else if(_targetT instanceof Type.Array && e.name().equals("length")) {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Deref(target.first(), e
					.name(), false, type,  e.attributes()),
					target.second());
		} else {
			syntax_error("cannot dereference type " + _targetT,e);
		}
		return null; // dead code		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doArrayIndex(Expr.ArrayIndex e) {
		Pair<JilExpr,List<JilStmt>> target = doExpression(e.target());
		Pair<JilExpr,List<JilStmt>> index = doExpression(e.index());
		Type type = e.attribute(Type.class);
		
		List<JilStmt> r = target.second();
		
		if(index.second().isEmpty()) {
			// easy case when no side-effects in index expression.
			
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.ArrayIndex(target.first(),
					index.first(), type, e.attributes()), r);
		} else {
			// harder case, there are side-effects in the index expression.
			JilExpr.Variable tmpVar = new JilExpr.Variable(getTempVar(),
					target.first().type(), e.attributes());
			r.add(new JilStmt.Assign(tmpVar,target.first(),e.attributes()));
			r.addAll(index.second());
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.ArrayIndex(tmpVar,
					index.first(), type, e.attributes()), r);
		}
	}
	
	protected Pair<JilExpr,List<JilStmt>> doNew(Expr.New e) {
		// Second, recurse through any parameters supplied ...
		ArrayList<JilStmt> r = new ArrayList();	
		Type.Reference type = e.type().attribute(Type.Reference.class);
				
		MethodInfo mi = e.attribute(MethodInfo.class);			
		
		Pair<JilExpr,List<JilStmt>> context = doExpression(e.context());
		Pair<List<JilExpr>,List<JilStmt>> params = doExpressionList(e.parameters());
								
		if(context != null) {
			r.addAll(context.second());
		}
		
		r.addAll(params.second());					
		
		if(mi != null) {			
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.New(type, params
					.first(), mi.type, e.attributes()), r);
		} else if(type instanceof Type.Array){
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.New(type,
					params.first(), new Type.Function(Types.T_VOID), e
							.attributes()), r);
		} else {
			syntax_error("internal failure --- unable to find method information",e);
			return null;
		}
	}
	
	protected Pair<JilExpr,List<JilStmt>> doInvoke(Expr.Invoke e) {
		ArrayList<JilStmt> r = new ArrayList();
		Type type = e.attribute(Type.class);				
		MethodInfo mi = e.attribute(MethodInfo.class);				
		
		Pair<JilExpr,List<JilStmt>> target = doExpression(e.target());
		r.addAll(target.second());
		
		Pair<List<JilExpr>,List<JilStmt>> params = doExpressionList(e.parameters());
		r.addAll(params.second());
		
		JilExpr rec = target.first();
		
		if (rec instanceof JilExpr.ClassVariable) {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Invoke(target.first(), e
					.name(), params.first(), mi.type, type, e
					.attributes()), r);
		} else if (rec instanceof JilExpr.Variable
				&& ((JilExpr.Variable) rec).value().equals("super")) {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.SpecialInvoke(target
					.first(), e.name(), params.first(), mi.type, type, e
					.attributes()), r);
		} else if (rec.type() instanceof Type.Array && e.name().equals("clone")) {
			// this is a special case for array cloning. It should really be in
			// TypePropagation, but the problem is that implicit cast needs to
			// be on the returned expression, which type propagation does not
			// support.
			JilExpr ie = new JilExpr.Invoke(target.first(), e.name(), params
					.first(), mi.type, type, e.attributes());
			ie = new JilExpr.Cast(ie, rec.type());
			return new Pair<JilExpr, List<JilStmt>>(ie, r);
		} else {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Invoke(target.first(), e
					.name(), params.first(), mi.type, type, e
					.attributes()), r);
		}
	}
	
	protected Pair<JilExpr,List<JilStmt>> doInstanceOf(Expr.InstanceOf e) {
		Pair<JilExpr,List<JilStmt>> lhs = doExpression(e.lhs());
		Type type = e.attribute(Type.class);
		Type.Reference rhs = e.rhs().attribute(Type.Reference.class);
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.InstanceOf(lhs.first(), rhs,
				type, e.attributes()), lhs.second());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doCast(Expr.Cast e) {
		Pair<JilExpr,List<JilStmt>> expr = doExpression(e.expr());		
		Type type = e.attribute(Type.class);
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Cast(expr.first(),
				type, e.attributes()), expr.second());		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doConvert(Expr.Convert e) {
		Pair<JilExpr,List<JilStmt>> expr = doExpression(e.expr());		
		Type.Primitive type = e.attribute(Type.Primitive.class);
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Convert(type, expr.first(),
				e.attributes()), expr.second());		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doBoolVal(Value.Bool e) {
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Bool(e.value()),
				new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doCharVal(Value.Char e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Char(e.value()), new ArrayList<JilStmt>());		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doByteVal(Value.Byte e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Byte(e.value()), new ArrayList<JilStmt>());		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doShortVal(Value.Short e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Short(e.value()), new ArrayList<JilStmt>());		
	}
	
	protected Pair<JilExpr,List<JilStmt>> doIntVal(Value.Int e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Int(e.value()), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doLongVal(Value.Long e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Long(e.value()), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doFloatVal(Value.Float e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Float(e.value()), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doDoubleVal(Value.Double e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Double(e.value()), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doStringVal(Value.String e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.StringVal(e.value()), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doNullVal(Value.Null e) {
		return new Pair<JilExpr,List<JilStmt>>(new JilExpr.Null(), new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doTypedArrayVal(Value.TypedArray e) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		Type.Array type = e.attribute(Type.Array.class);		
		Pair<List<JilExpr>,List<JilStmt>> exprs = doExpressionList(e.values());
		r.addAll(exprs.second());		
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Array(
				exprs.first(), type, e.attributes()), r);
	}
	
	protected Pair<JilExpr,List<JilStmt>> doArrayVal(Value.Array e) {
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();		
		Type.Array type = e.attribute(Type.Array.class);
		Pair<List<JilExpr>,List<JilStmt>> exprs = doExpressionList(e.values());
		r.addAll(exprs.second());
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Array(
				exprs.first(), type, e.attributes()), r);
	}
	
	protected Pair<JilExpr,List<JilStmt>> doClassVal(Value.Class e) {
		Type classType = e.value().attribute(Type.class);	
		Type.Clazz type = e.attribute(Type.Clazz.class);
		
		if(type instanceof Type.Clazz) {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Class(
					classType, type, e.attributes()),
					new ArrayList<JilStmt>());
		} else {
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Class(
					classType, JAVA_LANG_OBJECT, e.attributes()),
					new ArrayList<JilStmt>());
		}
	}
		
	protected Pair<JilExpr, List<JilStmt>> doLocalVariable(
			Expr.LocalVariable e) {
		Type type = e.attribute(Type.class);
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.Variable(e.value(), type, e
				.attributes()), new ArrayList<JilStmt>());
	}

	protected Pair<JilExpr, List<JilStmt>> doNonLocalVariable(
			Expr.NonLocalVariable e) {
		syntax_error(
				"internal failure (support for non-local variables not implemented!)",
				e);
		return null;
	}
	
	protected Pair<JilExpr,List<JilStmt>> doClassVariable(Expr.ClassVariable e) {
		Type.Clazz type = e.attribute(Type.Clazz.class);
		return new Pair<JilExpr, List<JilStmt>>(new JilExpr.ClassVariable(type, e.attributes()),
				new ArrayList<JilStmt>());
	}
	
	protected Pair<JilExpr,List<JilStmt>> doUnOp(Expr.UnOp e) {		
		Pair<JilExpr, List<JilStmt>> r = doExpression(e.expr());		
		Type.Primitive type = e.attribute(Type.Primitive.class);
		List<JilStmt> stmts = r.second();

		switch (e.op()) {
		case Expr.UnOp.PREDEC:
		{				
			JilExpr lhs = r.first();
			JilExpr rhs = new JilExpr.BinOp(lhs, constant(1,lhs.type()), JilExpr.BinOp.SUB,
					type, e.attributes());
			stmts.add(new JilStmt.Assign(lhs,rhs,e.attributes()));
			return new Pair<JilExpr, List<JilStmt>>(r.first(),stmts);		
		}
		case Expr.UnOp.PREINC:
		{
			JilExpr lhs = r.first();
			JilExpr rhs = new JilExpr.BinOp(lhs, constant(1,lhs.type()), JilExpr.BinOp.ADD,
					type, e.attributes());
			stmts.add(new JilStmt.Assign(lhs,rhs,e.attributes()));
			return new Pair<JilExpr, List<JilStmt>>(lhs,stmts);
		}
		case Expr.UnOp.POSTINC:
		{
			JilExpr lhs = r.first();	
			JilExpr.Variable tmp = new JilExpr.Variable(getTempVar(),type,new ArrayList(lhs.attributes()));			
			stmts.add(new JilStmt.Assign(tmp,lhs,e.attributes()));			
			JilExpr rhs = new JilExpr.BinOp(lhs, constant(1,lhs.type()), JilExpr.BinOp.ADD,
					type, e.attributes());
			stmts.add(new JilStmt.Assign(lhs,rhs,e.attributes()));
			return new Pair<JilExpr, List<JilStmt>>(tmp,stmts);		
		}
		case Expr.UnOp.POSTDEC:
		{
			JilExpr lhs = r.first();
			JilExpr.Variable tmp = new JilExpr.Variable(getTempVar(),type,new ArrayList(lhs.attributes()));			
			stmts.add(new JilStmt.Assign(tmp,lhs,e.attributes()));			
			JilExpr rhs = new JilExpr.BinOp(lhs, constant(1,lhs.type()), JilExpr.BinOp.SUB,
					type, e.attributes());
			stmts.add(new JilStmt.Assign(lhs,rhs,e.attributes()));
			return new Pair<JilExpr, List<JilStmt>>(tmp,stmts);
		}
		default:
			return new Pair<JilExpr, List<JilStmt>>(new JilExpr.UnOp(r.first(), e.op(),
					type, e.attributes()), r.second());
		}		
	}
		
	protected Pair<JilExpr,List<JilStmt>> doBinOp(Expr.BinOp e) {				
		
		// First, check for special string concatenation operator.
		if(e.op() == Expr.BinOp.CONCAT) {
			return doStringConcat(e);
		}
		
		Pair<JilExpr,List<JilStmt>> lhs = doExpression(e.lhs());
		Pair<JilExpr,List<JilStmt>> rhs = doExpression(e.rhs());

		Type type = e.attribute(Type.class);

		if(type instanceof Type.Primitive) {
			Type.Primitive ptype = (Type.Primitive) type;
			List<JilStmt> r = lhs.second();

			if(rhs.second().isEmpty()) {
				// This is the easy case: there are no side-effects in the rhs.
				r.addAll(rhs.second());

				return new Pair<JilExpr, List<JilStmt>>(new JilExpr.BinOp(lhs.first(), rhs
						.first(), e.op(), ptype, e.attributes()), r);
			} else {
				// This is the harder case: we have side-effects in the rhs.
				// Now, to deal with this i'm going to be a little conservative
				// and use a temporary variable. In some cases, it may be
				// possible to avoid the temporary variable, but for simplicity
				// I don't do this yet.  [NOTE, IT's HARD TO AVOID THE TMP]
				JilExpr.Variable tmpVar = new JilExpr.Variable(getTempVar(),
						lhs.first().type(), e.attributes());
				r.add(new JilStmt.Assign(tmpVar,lhs.first(),e.attributes()));
				r.addAll(rhs.second());
				return new Pair<JilExpr, List<JilStmt>>(new JilExpr.BinOp(tmpVar, rhs
						.first(), e.op(), ptype, e.attributes()), r);
			}
		} else {
			syntax_error(
					"internal failure --- problem processing binary operator ("
							+ lhs.first().type() + ")", e);
			return null;
		}
	}
	
	protected static int stringbuilder_label = 0;
	
	protected Pair<JilExpr,List<JilStmt>> doStringConcat(Expr.BinOp bop){
		
		// This method is evidence as to why Java sucks as a programming
		// language. It should be easy to construct datatypes, as I'm doing
		// here, but lack of good notation makes it awkward in Java. Sure, there
		// are some hacks to can do to improve the situation but basically it's
		// screwed.
		String builderLab = "$builder" + stringbuilder_label++;
		Pair<JilExpr,List<JilStmt>> lhs = doExpression(bop.lhs());
		Pair<JilExpr,List<JilStmt>> rhs = doExpression(bop.rhs());
		
		List<JilStmt> stmts = lhs.second();		
		stmts.addAll(rhs.second());
		
		Type.Clazz builder = new Type.Clazz("java.lang",
				"StringBuilder");
						
		stmts.add(new JilStmt.Assign(new JilExpr.Variable(builderLab, builder),
				new JilExpr.New(builder, new ArrayList<JilExpr>(),
						new Type.Function(T_VOID), bop.attributes()), bop
						.attributes()));					
		
		Type lhs_t = lhs.first().type(); 
		if (lhs_t instanceof Type.Primitive || isString(lhs_t)) {
			ArrayList<JilExpr> params = new ArrayList<JilExpr>();
			params.add(lhs.first());
			stmts.add(new JilExpr.Invoke(new JilExpr.Variable(builderLab,
					builder), "append", params, new Type.Function(
					new Type.Clazz("java.lang", "StringBuilder"), lhs.first()
							.type()), new Type.Clazz("java.lang",
					"StringBuilder"), bop.attributes()));
		} else {
			ArrayList<JilExpr> params = new ArrayList<JilExpr>();
			params.add(lhs.first());
			stmts.add(new JilExpr.Invoke(new JilExpr.Variable(builderLab,
					builder), "append", params, new Type.Function(
					new Type.Clazz("java.lang", "StringBuilder"),
					JAVA_LANG_OBJECT), new Type.Clazz("java.lang",
					"StringBuilder"), bop.attributes()));
		}

		// Now, do the right hand side
		JilExpr r;
		Type rhs_t = rhs.first().type(); 
		if(rhs_t instanceof Type.Primitive || isString(rhs_t)) {
			ArrayList<JilExpr> params = new ArrayList<JilExpr>();
			params.add(rhs.first());
			r = new JilExpr.Invoke(new JilExpr.Variable(builderLab, builder),
					"append", params, new Type.Function(new Type.Clazz(
							"java.lang", "StringBuilder"), rhs_t),
					new Type.Clazz("java.lang", "StringBuilder"), bop
							.attributes());
		} else {
			ArrayList<JilExpr> params = new ArrayList<JilExpr>();
			params.add(rhs.first());
			r = new JilExpr.Invoke(new JilExpr.Variable(builderLab, builder),
					"append", params, new Type.Function(new Type.Clazz(
							"java.lang", "StringBuilder"), JAVA_LANG_OBJECT),
					new Type.Clazz("java.lang", "StringBuilder"), bop
							.attributes());
		}

		r = new JilExpr.Invoke(r, "toString", new ArrayList<JilExpr>(),
				new Type.Function(JAVA_LANG_STRING), JAVA_LANG_STRING, bop
						.attributes());
		
		return new Pair<JilExpr,List<JilStmt>>(r,stmts);
	}

	protected int ternop_label = 0;
	protected Pair<JilExpr,List<JilStmt>> doTernOp(Expr.TernOp e) {
		String trueLab = "$ternoptrue" + ternop_label;
		String exitLab = "$ternopexit" + ternop_label++;
		Type r_t = e.attribute(Type.class);
		Pair<JilExpr,List<JilStmt>> cond = doExpression(e.condition());
		Pair<JilExpr,List<JilStmt>> tbranch = doExpression(e.trueBranch());
		Pair<JilExpr,List<JilStmt>> fbranch = doExpression(e.falseBranch());
		ArrayList<JilStmt> r = new ArrayList<JilStmt>();
		JilExpr.Variable tmp = new JilExpr.Variable(getTempVar(),r_t,e.attributes());
		r.addAll(cond.second());
		r.add(new JilStmt.IfGoto(cond.first(),trueLab,e.attributes()));
		r.addAll(fbranch.second());
		r.add(new JilStmt.Assign(tmp,fbranch.first(),e.attributes()));
		r.add(new JilStmt.Goto(exitLab,e.attributes()));
		r.add(new JilStmt.Label(trueLab,e.attributes()));
		r.addAll(tbranch.second());
		r.add(new JilStmt.Assign(tmp,tbranch.first(),e.attributes()));		
		r.add(new JilStmt.Label(exitLab,e.attributes()));		
		return new Pair(tmp,r);
	}
	
	/**
	 * The purpose of this method is to simplify the processing of an expression
	 * list. This is particular complex in the case of side-effecting
	 * statements.
	 */
	protected Pair<List<JilExpr>,List<JilStmt>> doExpressionList(List<Expr> exprs) {
		ArrayList<JilExpr> nexprs = new ArrayList();
		ArrayList<JilStmt> nstmts = new ArrayList();
		boolean hasSideEffects = false;
		for(int i=exprs.size()-1;i>=0;--i) {
			Expr p = exprs.get(i);
			Pair<JilExpr,List<JilStmt>> tmp = doExpression(p);
			if(hasSideEffects) {
				JilExpr.Variable var = new JilExpr.Variable(getTempVar(), tmp
						.first().type(), p.attributes());
				nstmts.add(0,new JilStmt.Assign(var,tmp.first(),p.attributes()));
				nexprs.add(0,var);
			} else {
				nexprs.add(0,tmp.first());	
			}			
			nstmts.addAll(0,tmp.second());
			if(!tmp.second().isEmpty()) {
				hasSideEffects=true;
			}
		}				
		return new Pair<List<JilExpr>,List<JilStmt>>(nexprs,nstmts);
	}
	
	/**
	 * This method determines whether or not it is possible for a statement to
	 * throw a particular exception. Observe that this determination is from a
	 * static point of view; thus, it may be possible with more complex
	 * reasoning to determine that a statement cannot throw an exception.
	 * However, we need to be somewhat conservative here to ensure that the
	 * bytecode produced will in fact pass the JVM bytecode verifier.
	 * 
	 * @param stmt
	 * @param exception
	 * @return
	 */
	protected boolean canThrowException(JilStmt stmt, Type.Clazz exception)
			throws ClassNotFoundException {
		
		if (types.subtype(JAVA_LANG_VIRTUALMACHINEERROR,
				exception, loader)) {
			// must treat these exceptions very conservatively, spec JVM spec
			// dictates they can occur at any point during exceptions.
			return true;
		}
		// now, try to eliminate all statements which definitely cannot
		// throw an exception.		
		if (stmt instanceof JilStmt.Goto || stmt instanceof JilStmt.Label
				|| stmt instanceof JilStmt.Nop) {
			return false;
		}
		// Now, tackle the easier ones.
		if (stmt instanceof JilStmt.Lock || stmt instanceof JilStmt.Unlock) {
			// these statements can only throw null pointer exceptions.
			// Actually, can also throw IllegaMonitorStateException
			return types.subtype(exception, JAVA_LANG_NULLPOINTEREXCEPTION,
					loader);			
		}
		
		if (stmt instanceof JilStmt.Throw) {
			// this can throw null pointer exception if the argument is null.
			// can also throw IllegalMonitorStateException (see JVM spec athrow)
			JilStmt.Throw tr = (JilStmt.Throw) stmt;
			return types.subtype(exception, tr.expr().type(), loader)
					|| types.subtype(exception, JAVA_LANG_NULLPOINTEREXCEPTION,
							loader);
		}
		
		ArrayList<JilExpr> exprs = new ArrayList();
		if (stmt instanceof JilExpr.Invoke) {
			// Some possible issue with respect to throwing Errors if
			// this causes class loading. See JVM Section 2.17.5.
			//
			// Also, can throw IncompatibleClassChangeError, IllegalAccessError,
			// AbstractMethodError, UnsatisfiedLinkError.
			if (types.subtype(exception, JAVA_LANG_RUNTIMEEXCEPTION, loader)
					|| types.subtype(JAVA_LANG_RUNTIMEEXCEPTION, exception,
							loader)) {
				return true;
			}

			// check declared exceptions
			MethodInfo mi = stmt.attribute(MethodInfo.class);
			for(Type.Clazz ex : mi.exceptions) {
				if (types.subtype(exception, ex, loader)) {
					return true;
				}
			}
			
			JilExpr.Invoke ivk = (JilExpr.Invoke) stmt;	
			exprs.addAll(ivk.parameters());			
		} else if (stmt instanceof JilExpr.New) {
			if (types.subtype(exception, JAVA_LANG_RUNTIMEEXCEPTION, loader)
					|| types.subtype(JAVA_LANG_RUNTIMEEXCEPTION, exception, loader)) {
				return true;
			}
			JilExpr.New ivk = (JilExpr.New) stmt;
			if (ivk.type() instanceof Type.Array
					&& types.subtype(exception, new Type.Clazz("java.lang",
							"NegativeArraySizeException"), loader)) {
				// In some cases, we can certain figure out that this cannot
				// happen.
				return true;			
			} else if(!(ivk.type() instanceof Type.Array)) {
				// check declared exceptions
				MethodInfo mi = ivk.attribute(MethodInfo.class);
				for(Type.Clazz ex : mi.exceptions) {
					if (types.subtype(exception, ex, loader)) {
						return true;
					}
				}
			}
			
			// Need to do something about checked exceptions. Also, if static
			// method then cannot throw NullPointException
			exprs.addAll(ivk.parameters());
		} else if(stmt instanceof JilStmt.Return) {
			JilStmt.Return r = (JilStmt.Return) stmt;
			// can also throw IllegalMonitorStateException (see JVM spec areturn)
			if(r.expr() == null) {
				return false;
			} else {
				exprs.add(r.expr());
			}
		} else if(stmt instanceof JilStmt.Assign) {
			JilStmt.Assign r = (JilStmt.Assign) stmt;
			if (r.lhs() instanceof JilExpr.ArrayIndex
					&& types.subtype(exception, new Type.Clazz("java.lang",
							"ArrayStoreException"), loader)) {
				return true;
			}
			exprs.add(r.lhs());
			exprs.add(r.rhs());
		} else if(stmt instanceof JilStmt.IfGoto) {
			JilStmt.IfGoto r = (JilStmt.IfGoto) stmt;
			exprs.add(r.condition());
		} else if(stmt instanceof JilStmt.Switch) {
			JilStmt.Switch r = (JilStmt.Switch) stmt;
			exprs.add(r.condition());
		}
		// Right, at this point, we have a bunch of expressions and we need to
		// check whether or not any of these could throw the exception in
		// question.
		for(JilExpr e : exprs) {
			if(canThrowException(e,exception)) {
				return true;
			}
		}
		return false;
	}
	
	protected boolean canThrowException(JilExpr expr, Type.Clazz exception)
			throws ClassNotFoundException {
		// reuse code above if possible
		if(expr instanceof JilExpr.Invoke || expr instanceof JilExpr.New) {
			return canThrowException((JilStmt)expr,exception);
		}
		
		// Again, build up an expression list to check
		ArrayList<JilExpr> exprs = new ArrayList();
		
		if(expr instanceof JilExpr.Cast) {
			JilExpr.Cast ec = (JilExpr.Cast) expr;
			if ((ec.type() instanceof Type.Reference || ec.type() instanceof Type.Null)
					&& types.subtype(exception, new Type.Clazz("java.lang",
							"ClassCastException"), loader)) {
				return true;
			}
			exprs.add(ec.expr());
		} else if(expr instanceof JilExpr.BinOp) {
			JilExpr.BinOp bop = (JilExpr.BinOp) expr;
			if (!(bop.type() instanceof Type.Float || bop.type() instanceof Type.Double)
					&& (bop.op() == JilExpr.BinOp.DIV || bop.op() == JilExpr.BinOp.MOD)
					&& types.subtype(exception, JAVA_LANG_ARITHMETICEXCEPTION,
							loader)) {
				// Curiously, divide-by-zero is only a problem for integer types.
				return true;
			}
			exprs.add(bop.lhs());
			exprs.add(bop.rhs());
		} else if(expr instanceof JilExpr.UnOp) {
			JilExpr.UnOp bop = (JilExpr.UnOp) expr;			
			exprs.add(bop.expr());				
		} else if(expr instanceof JilExpr.Deref) {
			// Some possible issue with respect to throwing Errors if this
			// instruction causes class loading. See JVM Section 2.17.5.
			JilExpr.Deref def = (JilExpr.Deref) expr;			
			if (types.subtype(exception, JAVA_LANG_NULLPOINTEREXCEPTION, loader)) {
				return true;
			}
			exprs.add(def.target());				
		} else if(expr instanceof JilExpr.Array) {
			JilExpr.Array arr = (JilExpr.Array) expr;
			exprs.addAll(arr.values());
		} else if(expr instanceof JilExpr.ArrayIndex) {
			JilExpr.ArrayIndex ai = (JilExpr.ArrayIndex) expr;
			if (types.subtype(exception, JAVA_LANG_NULLPOINTEREXCEPTION, loader)
					|| types.subtype(exception, new Type.Clazz("java.lang",
							"ArrayIndexOutOfBoundsException"), loader)) {
				return true;
			}
			exprs.add(ai.target());
			exprs.add(ai.index());
		} else if(expr instanceof JilExpr.InstanceOf) {
			JilExpr.InstanceOf iof = (JilExpr.InstanceOf) expr;
			exprs.add(iof.lhs());
		} 

		// Right, at this point, we have a bunch of expressions and we need to
		// check whether or not any of these could throw the exception in
		// question.
		for (JilExpr e : exprs) {
			if (canThrowException(e, exception)) {
				return true;
			}
		}

		return false;
	}
		
	protected Scope findEnclosingScope(Class c) {
		for(int i=scopes.size()-1;i>=0;--i) {
			Scope s = scopes.get(i);
			if(c.isInstance(s)) {
				return s;
			}
		}
		return null;
	}	
	
	protected int findSuperCall(List<JilStmt> stmts) {
		int r = 0;
		for(JilStmt stmt : stmts) {
			if(stmt instanceof JilExpr.Invoke) {
				JilExpr.Invoke sc = (JilExpr.Invoke) stmt;
				if (sc.name().equals("super") || sc.name().equals("this")) {
					return r;
				}	
			}
			r=r+1;
		}
		return -1;
	}
	
	public JilMethod createStaticInitialiser(JilClass parent) {
		List<JilMethod> si = parent.methods("<clinit>");
		if(si.size() == 0) {		
			ArrayList<Modifier> mods = new ArrayList<Modifier>();
			mods.add(Modifier.ACC_STATIC);
			JilMethod r = new JilMethod("<clinit>", new Type.Function(
					Types.T_VOID), new ArrayList(), mods,
					new ArrayList<Type.Clazz>());
			r.body().add(new JilStmt.Return(null));
			parent.methods().add(r);
			return r;
		} else {
			// It should be impossible to have more than one.
			return si.get(0);
		}
	}	
	
	protected JilExpr constant(int constant, Type t) {
		if(t instanceof Type.Byte) {
			return new JilExpr.Byte((byte)constant);
		} else if(t instanceof Type.Char) {
			return new JilExpr.Char((char)constant);
		} else if(t instanceof Type.Short) {
			return new JilExpr.Short((short)constant);
		} else if(t instanceof Type.Int) {
			return new JilExpr.Int(constant);
		} else if(t instanceof Type.Long) {
			return new JilExpr.Long((long)constant);
		} else if(t instanceof Type.Float) {
			return new JilExpr.Float((float)constant);
		} else {
			return new JilExpr.Double((double)constant);
		}
	}
	
	protected int tmp_label = 0;
	protected String getTempVar() {
		return "$tmp" + tmp_label++;
	}
	
	/**
     * Check wither a given type is a reference to java.lang.String or not.
     * 
     * @param t
     * @return
     */
	protected static boolean isString(Type t) {
		if(t instanceof Type.Clazz) {
			Type.Clazz c = (Type.Clazz) t;
			 return c.pkg().equals("java.lang") && c.components().size() == 1
					&& c.components().get(0).first().equals("String");			
		}
		return false;
	}		
}
