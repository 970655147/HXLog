/**
 * file name : Test04_ForEval.java
 * created at : ����6:45:25 2016��8��11��
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.math.Eval;

public class Test04_ForEval {

	// ����Eval
	public static void main(String[] args) {
		
		// 1. �򵥵Ĳ���
//		String exp = "2 * ( 3 + 4 )";
//		String exp = "2 * ( 3 + 4 ) + (3 * 5)";
//		String exp = "3 * 2 * ( 3 + 4 )";
//		String exp = "3 + 3 * 2 * ( 3 + 4 ) + 2";
		String exp = "3 + 3 * 2 * ( 3 + (3 * 6) + 4 ) + 2";
//		String exp = "3 + expection + 3 * 2 * ( 3 + (3 * 6) + 4 ) + 2";
//		String exp = "3 * 80 + 81 * 93";
//		String exp = "3 + 3 * 2 * ( 3 + 4 )";
//		String exp = "(3 * 2) * ( 3 + 4 )";
		// ...
//		String exp = "91 * 32 - 8 - 2";
		
		int val = Eval.eval(exp);
		
		System.out.println(val);
		
	}
	
}
