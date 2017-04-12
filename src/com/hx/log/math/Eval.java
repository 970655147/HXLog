/**
 * file name : Eval.java
 * created at : ����6:41:13 2016��8��11��
 * created by 970655147
 */

package com.hx.log.math;

import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

// ʵ��������js��eval���� [������ʽ����]
public final class Eval {
	
	// disable constructor
	private Eval() {
		Tools.assert0("can't instantiate !");
	}
	
	// �������ŵĳ���, ƥ��ķ���ӳ��, �������ŵ����ȼ�
	public static final char ADD = '+';
	public static final char SUB = '-';
	public static final char MUL = '*';
	public static final char DIV = '/';
	public static final char MOD = '%';
	public static final char LEFT_BRACKET = '(';
	public static final char RIGHT_BRACKET = ')';
	private static final Map<Character, Character> matched = new HashMap<>();
	private static Map<Character, Integer> PRIORITIES = new HashMap<>();
	
    // ��ʼ��
	static {
		matched.put(LEFT_BRACKET, RIGHT_BRACKET);
		
		PRIORITIES.put(ADD, 0);
		PRIORITIES.put(SUB, 0);
		PRIORITIES.put(MUL, 1);
		PRIORITIES.put(DIV, 1);
		PRIORITIES.put(MOD, 1);
		PRIORITIES.put(LEFT_BRACKET, 2);
		PRIORITIES.put(RIGHT_BRACKET, 2);
	}
	
    // ��ͳ�Ƴ�exp�и������ŵ�λ��  ����eval0��������
    // ���ȼ�¼���еķ��ŵ�λ��, ��ӵ�һ�������б���[��� ���Ӽ������, �ݹ����]
    // Ȼ��  ��������ı��ʽ
	public static int eval(String exp) {
		Tools.assert0(exp != null, "exp can't be null !");
		
		// Operator, Operation
		List<Integer> resInBrackets = new LinkedList<>();
		List<Operator> optr = new LinkedList<Operator>();
		for(int i=0; i<exp.length(); i++) {
			char c = exp.charAt(i);
			switch (c ) {
				case ADD :
				case SUB :
				case MUL :
				case DIV :
				case MOD :
					optr.add(new Operator(c, i));
					break;
				case LEFT_BRACKET :
					int nextI = getNextMatched(exp, i+1, LEFT_BRACKET, matched);
					resInBrackets.add(eval(exp.substring(i+1, nextI)) );
					optr.add(new Operator(LEFT_BRACKET, i) );
					optr.add(new Operator(RIGHT_BRACKET, nextI) );
					i = nextI;
				default :
					break;
			}
		}
		
//		while(optr.size() > 0) {
//			System.out.println(optr.pop());
//		}
		
		int val = eval0(exp, optr, resInBrackets, null);
		
		
		return val;
	}
	
    // ��������ı��ʽ  ��"�� -> β"
    // ��ȡ��һ����������ΪĬ�ϵķ���ֵ
        // Ȼ�� ��ȡ��һ��������, �Լ���һ��������
        // �����һ�������������ȼ����ڵ�ǰ������, �ݹ����(��ǰ���, ��һ��������֮��Ĳ���) [�������Ϊ�����������������, ����return�ĵݹ鹹���β�ݹ�, ��������  �Ͳ�����]
            // ����  �����һ���������Աߵ�����������, ѭ��, ֱ���������б���ֻʣ��һ��������
    // �������һ��������  ���ߵĲ�����
	private static int eval0(String exp, List<Operator> optr, List<Integer> resInBrackets, Operator lastOptNow) {
		if(Tools.isEmpty(optr) ) {
			return Integer.parseInt(exp);
		}
		
		int res = 0;
		if(lastOptNow == null) {
			Operator firstOpt = optr.get(0);
			if(firstOpt.oper == LEFT_BRACKET) {
				optr.remove(0);
				optr.remove(0);
				res = resInBrackets.remove(0);
			} else {
				res = parseFirstInt(exp, firstOpt );
			}
		} else {
			res = parseInt(exp, lastOptNow, optr.get(0) );
		}
		
		while(optr.size() > 1) {
			Operator optNow = optr.get(0);
			Operator optNext = optr.get(1);
			optr.remove(0);
			if(PRIORITIES.get(optNext.oper) > PRIORITIES.get(optNow.oper) ) {
				if(optNext.oper == LEFT_BRACKET) {
					Operator optNNext = null;
					if(optr.size() > 2) {
						optNNext = optr.get(2);
					}
					if((optNNext != null) && (PRIORITIES.get(optNNext.oper) > PRIORITIES.get(optNow.oper)) ) {
						return calc(res, optNow, eval0(exp, optr, resInBrackets, null) );
					} else {
						optr.remove(0);
						optr.remove(0);
						res = calc(res, optNow, resInBrackets.remove(0) );
					}
				} else {
					return calc(res, optNow, eval0(exp, optr, resInBrackets, optNow) );
				}
			} else {
				res = calc(res, optNow, parseInt(exp, optNow, optNext) );
			}
		}
		
		if(optr.size() > 0) {
			res = calc(res, optr.get(0), parseLastInt(exp, optr.get(0)) );
		}
		
		return res;
	}
    // ��������ı��ʽ  ��"β -> ��"
//	private static int eval0(String exp, List<Operator> optr, List<Integer> resInBrackets, Operator lastOptNow) {
//		int res = 0;
//		if(lastOptNow == null) {
//			Operator lastOpt = optr.get(optr.size()-1);
//			if(lastOpt.oper == RIGHT_BRACKET) {
//				optr.remove(optr.size()-1);
//				optr.remove(optr.size()-1);
//				res = resInBrackets.remove(resInBrackets.size()-1);
//			} else {
//				res = parseLastInt(exp, lastOpt );
//			}
//		} else {
//			res = parseInt(exp, lastOptNow, optr.get(optr.size()-1) );
//		}
//		while(optr.size() > 1) {
//			Operator optNow = optr.get(optr.size()-1);
//			Operator optNext = optr.get(optr.size()-2);
//			optr.remove(optr.size()-1);
//			if(PRIORITIES.get(optNext.oper) > PRIORITIES.get(optNow.oper) ) {
//				if(optNext.oper == RIGHT_BRACKET) {
//					Operator optNNext = optr.get(optr.size()-3);
//					if(PRIORITIES.get(optNNext.oper) > PRIORITIES.get(optNow.oper) ) {
//						return calc(eval0(exp, optr, resInBrackets, null), optNow, res );
//					} else {
//						optr.remove(optr.size()-1);
//						optr.remove(optr.size()-1);
//						res = calc(resInBrackets.remove(resInBrackets.size()-1), optNow, res );
//					}
//				} else {
//					return calc(eval0(exp, optr, resInBrackets, optNow), optNow, res );
//				}
//			} else {
//				res = calc(parseInt(exp, optNow, optNext), optNow, res );
//			}
//		}
//		if(optr.size() > 0) {
//			res = calc(parseFirstInt(exp, optr.get(0)), optr.get(0), res );
//		}
//		
//		return res;
//	}

    // ����val01, val02  ��opt����  ���ؽ��
	private static int calc(int val01, Operator opt, int val02) {	
		int val = 0;
		switch(opt.oper) {
			case ADD :
				val = val01 + val02;
				break;
			case SUB :
				val = val01 - val02;
				break;
			case MUL :
				val = val01 * val02;
				break;
			case DIV :
				val = val01 / val02;
				break;
			case MOD :
				val = val01 % val02;
				break;
		}
		
		return val;
	}
	
    // ������ǰ����� ����һ�������֮��ĵĲ�����
	private static int parseInt(String exp, Operator optNow, Operator optNext) {
		String intStr = null;
		try {
			intStr = exp.substring(optNow.index+1, optNext.index).trim();
			return Integer.parseInt(intStr);
		} catch (Exception e) {
			Log.err("error while parse first operand[idx : " + optNow.index + "][' " + intStr + " '] !");
			throw e;
		}
	}
	private static int parseLastInt(String exp, Operator optNow) {
		try {
			return Integer.parseInt(exp.substring(optNow.index+1).trim() );
		} catch(Exception e) {
			Log.err("error while parse last operand[idx : " + optNow.index + "] !");
			throw e;
		}
	}
	private static int parseFirstInt(String exp, Operator optNow) {
		try {
			return Integer.parseInt(exp.substring(0, optNow.index).trim() );
		} catch(Exception e) {
			Log.err("error while parse first operand[idx : " + optNow.index + "] !");
			throw e;
		}
	}
	
    // ��ȡ��ǰλ�õķ���ƥ�����һ������
	private static int getNextMatched(String exp, int idx, char left, Map<Character, Character> matched) {
		Deque<Character> stack = new LinkedList<>();
		stack.push(left);
		for(int i=idx; i<exp.length(); i++) {
			char ch = exp.charAt(i);
			if(ch == matched.get(stack.peek()) ) {
				stack.pop();
				if(stack.size() == 0) {
					idx = i;
					break ;
				}
				continue ;
			}
			if(matched.containsKey(ch) ) {
				stack.push(ch);
			}
		}
		
		return idx;
	}
	
	// --------------- bean --------------------
    // ��װÿһ��������   �Լ�������
	private static class Operator {
        // ������, ����
		char oper;
		int index;
		
        // ��ʼ��
		public Operator() {
			
		}
		Operator(char oper, int index) {
			this.oper = oper;
			this.index = index;
		}
		
		// for debug ..
		public String toString() {
			return oper + " -> " + index + "; ";
		}
	}
	
}
