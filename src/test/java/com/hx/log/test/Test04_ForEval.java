/**
 * file name : Test04_ForEval.java
 * created at : 下午6:45:25 2016年8月11日
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.math.Eval;

public class Test04_ForEval {

	// 测试Eval
	public static void main(String[] args) {
		
		// 1. 简单的测试
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
