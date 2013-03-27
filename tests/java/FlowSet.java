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

package jkit.jil.dfa;

public interface FlowSet {

	/**
	 * FlowSets must be cloneable to facilitate multiple flows of execution
	 * from conditionals
	 * 
	 * @return A Clone of the current FlowSet
	 */
	public Object clone();
	
	/**
     * Computes the least upper bound of this flowset and that provided. <b>NOTE</b>
     * the join operation has a subtle, yet important, requirement. If the
     * result of the join must be equivalent to *this* flowset, then it must be
     * the *same* flowset.
     * 
     * @param s
     *            Another FlowSet to join with this
     * @return true if this FlowSet has changed due to the computation, false
     *         otherwise
     */
	public FlowSet join(FlowSet s);
}
