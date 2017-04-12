/**
 * file name : JSONExtractor.java
 * created at : ����10:29:32 2016��8��13��
 * created by 970655147
 */

package com.hx.log.json;

import static com.hx.log.util.Log.err;

import com.hx.log.interf.IdxIterator;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.hx.log.interf.IdxIterator.ChainOfIdxIterator;
import com.hx.log.interf.IdxIterator.NoneIdxIterator;
import com.hx.log.interf.IdxIterator.RangeIdxIterator;
import com.hx.log.interf.IdxIterator.SingleIdxIterator;
import com.hx.log.interf.IdxIterator.SomeBitIncIdxIterator;
import com.hx.log.interf.IdxIterator.UpperBoundsIdxIterator;

import com.hx.log.math.Eval;
import com.hx.log.util.Tools;
import com.hx.log.str.WordsSeprator;
import net.sf.json.JSON;
import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

// ͨ��������pattern, �Ӹ�����JSON����ȡ����
public final class JSONExtractor {

	// disable constructor
	private JSONExtractor() {
		Tools.assert0("can't instantiate !");
	}
	
	// ͨ���, '?'ƥ��һ�������ַ�, '*'ƥ���������ַ�
	public static final Character MATCH_ONE = '?';
	public static final Character MATCH_MULTI = '*';
	public static final char[] WILDCARDS = new char[]{MATCH_ONE, MATCH_MULTI };
	public static final int MATCH_ONE_IDX = 0;
	public static final int MATCH_MULTI_IDX = 1;
	
	// JSONExtractor��pattern���
	public static final String EVAL_STARTS = "eval#";
	public static final String $THIS = "$this";
	public static final String $LEN = "$len";
	public static final String $CONCATE = ".";
	public static final String $ARR_LEFT_BRACKET = "[";
	public static final String $ARR_RIGHT_BRACKET = "]";
	public static final String $ARR_RANGE_SEP = ",";
	public static final Set<String> OPERAND_SEPS = Tools.asSet($CONCATE, $ARR_RANGE_SEP, 
													$ARR_LEFT_BRACKET, $ARR_RIGHT_BRACKET);
	public static final Map<String, String> OPERAND_ESCAPE_MAP = Tools.asMap(
													new String[]{"'", "\"" },
													new String[]{"'", "\"" } );
	
	// ����extractPattern�ķָ���
	public static final String PATTERN_SEP = "\\|";
	
	/**
	 * @Name: extractInfoFromJSON 
	 * @Description: ���ݸ�����pattern, ��ȡ�������, ���Ӷ��ڶ��pattern�Ĵ���
	 * @param json
	 * @param pattern
	 * @return  
	 * @Create at 2016-09-30 22:57:33 by '970655147'
	 */
	public static JSONArray extractInfoFromJSON(JSON json, String pattern) {
		Tools.assert0(json != null, "json can't be null !");
		Tools.assert0(pattern != null, "pattern can't be null !");
		
		String[] subPatterns = pattern.split(PATTERN_SEP);
		for(int i=0; i<subPatterns.length; i++) {
			JSONArray res = extractInfoFromJSON0(json, subPatterns[i]);
			if(! Tools.isEmpty(res) ) {
				return res;
			}
		}
		
		return new JSONArray();
	}
	
	/**
	 * @Name: extractInfoFromJSON 
	 * @Description: ���ݸ�����pattern, ��ȡ�������
	 *   1. Ԥ��������, ����pattern����OperationChain
	 *   2. �������ҵ��['$this.category.xx', '$this']
	 *   3. ���ػ�ȡ��������
	 * @param json ��������Ҫ��ȡ���ݵ�json
	 * @param pattern ȷ��patternԤ����֮��ֻ�����һ��'*'
	 * @return  
	 * @Create at 2016��8��13�� ����10:30:15 by '970655147'
	 */
	private static JSONArray extractInfoFromJSON0(JSON json, String pattern) {
		pattern = Tools.trimAllSpaces(pattern);
		Operand head = parseOperand(pattern);
		
		JSONArray res = new JSONArray();
		Operand prev = head;
		Operand cur = prev.next;
		List<JSON> curJsonList = Tools.asList(json);
		// incase of '$this.category.xx'
		while(cur != null) {
			// cut off
			if(curJsonList.isEmpty() ) {
				break ;
			}
			
			// Collection's use may optimize [Queue instead?]
			List<JSON> nextJsonList = new ArrayList<>(curJsonList.size() );
			// 			  prev		   cur
			//              |			|
			// incase of '$this[1, 4].category.url'
			// incase of '$this[1, 4].category[2].url'
			// or collect data in 'operandChain of final'
			if(prev.isArray() ) {
				for(JSON curJson : curJsonList) {
					Tools.assert0(curJson.isArray(), "expect an JSONArray : " + curJson.toString() );
					
					// ...
					// ������arr[idx01][idx02], ���Ĭ��Լ��prevArr��ȡ���������ݾ�ΪJSONObject
						// ���prevArr�л�ȡ�������ΪJSONArray �����������ͻ��׳��쳣
						// ��� prevArrObj����JSON��ʽ�����ݽṹ, �׳��쳣
					JSONArray prevArr = (JSONArray) curJson;
					IdxIterator idxIterator = getIdxIteratorByOperand(prev, prevArr);
					// collect data
					if(cur.next == null) {
						collectPrevArr(prevArr, idxIterator, cur, res);
					// iterate update
					} else {
						while(idxIterator.hasNext() ) {
							JSONObject prevArrObj = (JSONObject) prevArr.get(idxIterator.next());
							if(! Tools.isEmpty(prevArrObj) ) {
								nextJsonList.add((JSON) prevArrObj.opt(cur.key) );
							}
						}
					}
				}
			// 			  prev     cur
			//              |		|
			// incase of '$this.category.url'
			// incase of '$this.category[2].url'
			// or collect data in 'operandChain of final' 
			} else {
				for(JSON curJson : curJsonList) {
					Tools.assert0(! curJson.isArray(), "expect an JSONObject : " + curJson.toString() );
					
					JSONObject prevObj = (JSONObject) curJson; 
					// collect data					
					if(cur.next == null) {
						collectPrevObj(prevObj, cur, res);
					// iterate update
					} else {
						if(cur.isArray() ) {
							curJson = prevObj.optJSONArray(cur.key);
						} else {
							curJson = prevObj.optJSONObject(cur.key);
						}
						if(curJson != null) {
							nextJsonList.add(curJson);
						}
					}
				}
			}
			curJsonList = nextJsonList;
			
			prev = cur;
			cur = cur.next;
		}
		// incase of '$this[xx, xx]', '$this'
		if(prev == head) {
			collect$This(curJsonList, prev, res);
		}
		
		return res;
	}
	
	/**
	 * @Name: getIdxIteratorByPattern 
	 * @Description: ���ݸ�����pattern, �Լ�����������ĳ���, ��ȡ������IdxIterator
	 * @param pattern
	 * @param len ������Ԫ�ظ���
	 * @return  
	 * @Create at 2016��8��13�� ����10:34:05 by '970655147'
	 */
	public static IdxIterator getIdxIteratorByPattern(String pattern, int len) {
		Tools.assert0(pattern != null, "pattern can't be null !");
		pattern = preparePattern(pattern);
		
		int lenLen = String.valueOf(len).length();
		int needCut = pattern.length() - lenLen;
		// too long, and can't compitable
		if(needCut > 0) {
			for(int i=0; i<needCut; i++) {
				if(! Tools.contains(WILDCARDS, pattern.charAt(i))) {
					return NoneIdxIterator.getInstance();
				}
			}
		}
		
		String trimedPattern = (needCut > 0) ? pattern.substring(needCut) : pattern;
		int idxOfStar = trimedPattern.indexOf(MATCH_MULTI);
		boolean containsStar = idxOfStar >= 0;
		if(! containsStar) {
			return getIdxIteratorByPattern0(pattern, len);
		// incase of contains '*', padding [1-maxPadding] '?', then concate the 'idxIterator' with 'ChainOfIdxIterator'
		} else {
			String before = trimedPattern.substring(0, idxOfStar);
			String after = trimedPattern.substring(idxOfStar+1);
			int maxPadding = lenLen - (pattern.length() - 1);
			
			ChainOfIdxIterator chain = new ChainOfIdxIterator();
			for(int i=1; i<=maxPadding; i++) {
				String paddinged = paddingQuestioned(before, after, String.valueOf(MATCH_ONE), i);
				chain.add(getIdxIteratorByPattern0(paddinged, len) );
			}
			return chain;
		}
	}
	
	// Ԥ����pattern
	// 1. ��ֹ���Ƶ�������� "**", "*?"
	public static String preparePattern(String pattern) {
		StringBuilder sb = new StringBuilder(pattern.length() );
		boolean starAppeared = false;
		for(int i=0, len=pattern.length(); i<len; i++) {
			char ch = pattern.charAt(i);

			// trimAll
			if(Character.isWhitespace(ch) ) {
				continue ;
			}
			if(! Tools.contains(WILDCARDS, ch) ) {
				sb.append(ch);
			// '??*??'				
			} else {
				int nextI = i+1;
				boolean containsStar = false;
				if(MATCH_MULTI == ch) {
					containsStar = true;
				}
				while((nextI < len) && (Tools.contains(WILDCARDS, pattern.charAt(nextI))) ) {
					if(MATCH_MULTI == pattern.charAt(nextI) ) {
						containsStar = true;
					}
					nextI ++;
				}
				
				if(containsStar) {
					sb.append(MATCH_MULTI);
					if(starAppeared) {
						throw new RuntimeException("'JSONExtractor' can only exists one '*' in pattern !");
					}
					starAppeared = true;
				} else {
					sb.append(pattern.substring(i, nextI) );
				}
				i = nextI - 1;
			}
		}
		
		return sb.toString();
	}	
	
	// ----------------------- assist method --------------------------------
	// ����pattern ����Operand��
	private static Operand parseOperand(String pattern) {
		WordsSeprator sep = new WordsSeprator(pattern, OPERAND_SEPS, OPERAND_ESCAPE_MAP, true);
		if(! sep.hasNext() ) {
			return null;
		}

		Tools.assert0($THIS.equals(sep.next() ), "pattern must starts with '$this' !");
		Operand head = new Operand($THIS, null, null, null);
		parseArrRange(sep, head);
		Operand prev = head;
		while(sep.hasNext() && $CONCATE.equals(sep.next()) ) {
			String curOpeName = sep.next();
			Operand curOpe = new Operand(curOpeName, null, null, null);
			parseArrRange(sep, curOpe);
			prev.next = curOpe;
			prev = curOpe;
		}
		if(sep.hasNext() && (! $CONCATE.equals(sep.seekLastNext())) ) {
			Tools.assert0("not good format around : '" + sep.rest() + "'");;
		}

		return head;
	}
	// ����������ope�����鷶Χ����[left, right] or [left]
	private static void parseArrRange(WordsSeprator sep, Operand ope) {
		if($ARR_LEFT_BRACKET.equals(sep.seek()) ) {
			sep.next();									// '['
			String ope01 = sep.next();
			String commonOrRightBracket = sep.next();	// ',', ']'
			if($ARR_RANGE_SEP.equals(commonOrRightBracket) ) {
				String ope02 = sep.next();
				Tools.assert0(! Tools.isEmpty(ope01), "leftOperand can't be null !");
				Tools.assert0(! Tools.isEmpty(ope02), "rightOperand can't be null !");
				
				ope.left = ope01;
				ope.right = ope02;
				Tools.assert0($ARR_RIGHT_BRACKET.equals(sep.next()), "expect a ']', around : '" + sep.rest() + "' ");
			} else if($ARR_RIGHT_BRACKET.equalsIgnoreCase(commonOrRightBracket) ) {
				Tools.assert0(! Tools.isEmpty(ope01), "leftOperand can't be null !");
				ope.left = ope01;
			} else {
				Tools.assert0("expect a ',' or ']', around : '" + sep.rest() + "' ");
			}
		}
	}
	
	// ���ݸ�����Operand, ����IdxIterator
	private static IdxIterator getIdxIteratorByOperand(Operand prev, JSONArray prevArr) {
		String left = prev.left, right = prev.right;
		IdxIterator res = null;
		// there are two bounds
		if(right != null) {
			// trim '' or ""
			left = trimQuote(left);
			right = trimQuote(right);
			if(left.startsWith(EVAL_STARTS) ) {
				// inject $len
				left = left.replace($LEN, String.valueOf(prevArr.size()) );
				if(right.startsWith(EVAL_STARTS) ) {
					right = right.replace($LEN, String.valueOf(prevArr.size()) );
					int start = Eval.eval(left.substring(EVAL_STARTS.length()) );
					int end = Eval.eval(right.substring(EVAL_STARTS.length()) );
					res = new RangeIdxIterator(start, end);
				} else {
					Tools.assert0("not compatiable useage 'eval' & 'wildcard' !");
				}
			} else {
				if(right.startsWith(EVAL_STARTS) ) {
					Tools.assert0("not compatiable useage 'eval' & 'wildcard' !");
				} else {
					int start = formatLeft(left, prevArr);
					int end = formatRight(right, prevArr);
					res = new RangeIdxIterator(start, end);
				}
			}
		// just only one bounds
		} else {
			left = trimQuote(left);
			if(left.startsWith(EVAL_STARTS) ) {
				left = left.replace($LEN, String.valueOf(prevArr.size()) );
				int start = Eval.eval(left.substring(EVAL_STARTS.length()) );
				res = new SingleIdxIterator(start);
			} else {
				res = getIdxIteratorByPattern(left, prevArr.size() );
			}
		}
		
		if(res == null) {
			return NoneIdxIterator.getInstance();
		}
		return legalization(res, prevArr);
	}
	// ʹ�ø����������Ϸ���
	private static IdxIterator legalization(IdxIterator idxIterator, JSONArray arr) {
		return new UpperBoundsIdxIterator(idxIterator, arr.size(), false);
	}
	// ��ʽ��[left, right], left, right[ԭ��Ϊ����ߵ�?��Ϊ0, �ұߵ�?��Ϊ9, ��ߵ�*ȥ��, �ұߵ�*���9]
	private static int formatLeft(String left, JSONArray arr) {
		StringBuilder sb = new StringBuilder(left.length() );
		for(int i=0, len=left.length(); i<len; i++) {
			char ch = left.charAt(i);
			if(ch == MATCH_MULTI) {
				continue ;
			}
			if(ch == MATCH_ONE) {
				sb.append("0");
			} else {
				sb.append(ch);
			}
		}
		// compatiable with '*'
		if(sb.length() == 0) {
			sb.append("0");
		}
		
		return Integer.parseInt(sb.toString() );
	}
	private static int formatRight(String right, JSONArray arr) {
		StringBuilder sb = new StringBuilder(right.length() );
		int lenLen = String.valueOf(arr.size()).length();
		int maxPadding = lenLen - (right.length() - 1);
		
		for(int i=0, len=right.length(); i<len; i++) {
			char ch = right.charAt(i);
			if(ch == MATCH_MULTI) {
				for(int paddingCnt=0; paddingCnt<maxPadding; i++) {
					sb.append("9");
				}
				continue ;
			}
			if(ch == MATCH_ONE) {
				sb.append("9");
			} else {
				sb.append(ch);
			}
		}
		
		return Integer.parseInt(sb.toString() );
	}
	// ���str�����ſ�ͷ��β, ��ȥ������
	private static String trimQuote(String str) {
		if((str.startsWith("'") && str.endsWith("'") )
				|| (str.startsWith("\"") && str.endsWith("\"")) ){
			str = str.substring(1, str.length()-1);
		}
		return str;
	}
	
	// û��*�ĳ���, ��ȡ������patternƥ������е�Idxƥ���IdxIterator
		// ������ƥ�䵽���ݵĳ���, �Լ���ȷƥ��ĳ���
		// ���ݶ�Ӧ��ͨ��� �Լ�pattern����SomeBitIncIdxIterator
	private static IdxIterator getIdxIteratorByPattern0(String pattern, int len) {
		int lenLen = String.valueOf(len).length();
		int needCut = pattern.length() - lenLen;
		// too long, and can't compitable
		if(needCut > 0) {
			for(int i=0; i<needCut; i++) {
				if(pattern.charAt(i) != MATCH_ONE) {
					return NoneIdxIterator.getInstance();
				}
			}
		}
		
		String trimedPattern = (needCut > 0) ? pattern.substring(needCut) : pattern;
		StringBuilder startBuilder = new StringBuilder(trimedPattern.length() );
		StringBuilder endBuilder = new StringBuilder(trimedPattern.length() );
		BitSet bs = new BitSet();
		boolean isSingle = true;
		// ??2		// higher -> lower
		for(int i=0, _len=trimedPattern.length(); i<_len; i++) {
			char ch = trimedPattern.charAt(i);
			if(ch == MATCH_ONE) {
				startBuilder.append("0");
				endBuilder.append("9");
				bs.set(_len-i-1);
				isSingle = false;
			} else {
				startBuilder.append(ch);
				endBuilder.append(ch);
			}
		}
		
		// accurate match
		int start = Integer.parseInt(startBuilder.toString() );
		if(isSingle) {
			return new SingleIdxIterator(start);
		}
		
		// nonAccurate match
		int end = Integer.parseInt(endBuilder.toString() );
		ChainOfIdxIterator chain = new ChainOfIdxIterator().add(new SomeBitIncIdxIterator(start, end, bs) ).add(new SingleIdxIterator(end) );
		return new UpperBoundsIdxIterator(chain, len);
	}
	
	// ��before, after֮�����times��padding
	private static String paddingQuestioned(String before, String after, String padding, int times) {
		StringBuilder sb = new StringBuilder(before.length() + after.length() + padding.length() * times);
		sb.append(before);
		for(int i=0; i<times; i++) {
			sb.append(padding);
		}
		sb.append(after);
		return sb.toString();
	}

	// �ռ�prevΪ���� ����Object�ĳ���������
	private static void collectPrevArr(JSONArray prevArr, IdxIterator idxIterator, Operand cur, JSONArray res) {
		if(cur.isArray() ) {
			// ...
			while(idxIterator.hasNext() ) {
//				JSONArray arr = (JSONArray) prevArr.get(idxIterator.next());
				JSONObject theNObj = (JSONObject) prevArr.get(idxIterator.next());
				JSONArray arr = theNObj.getJSONArray(cur.key);
				if(! Tools.isEmpty(arr) ) {
					IdxIterator curIdxIterator = getIdxIteratorByOperand(cur, arr);
					while(curIdxIterator.hasNext() ) {
						Object resEle = arr.opt(curIdxIterator.next());
						if(resEle != null) {
							res.add(resEle);
						}
					}
				} else {
					err("err while got " + prevArr);
				}
			}
		} else {
			while(idxIterator.hasNext() ) {
				JSONObject obj = (JSONObject) prevArr.get(idxIterator.next());
				if(! Tools.isEmpty(obj) ) {
					Object resEle = obj.opt(cur.key);
					if(resEle != null) {
						res.add(resEle);
					}
				} else {
					err("err while got " + prevArr);
				}
			}
		}
	}
	private static void collectPrevObj(JSONObject prevObj, Operand cur, JSONArray res) {
		if(cur.isArray() ) {
			// ...
			JSONArray arr = (JSONArray) prevObj.get(cur.key);
			if(! Tools.isEmpty(arr) ) {
				IdxIterator curIdxIterator = getIdxIteratorByOperand(cur, arr);
				while(curIdxIterator.hasNext() ) {
					Object resEle = arr.opt(curIdxIterator.next());
					if(resEle != null) {
						res.add(resEle);
					}
				}
			} else {
				err("err while got " + prevObj);
			}
		} else {
			Object resEle = prevObj.opt(cur.key);
			if(resEle != null) {
				res.add(resEle);
			}
		}
	}
	// ���� '$this[xx, xx]', '$this' �ĳ���
	private static void collect$This(List<JSON> curJsonList, Operand prev, JSONArray res) {
		if(! Tools.isEmpty(curJsonList) ) {
			if(prev.isArray() ) {
				for(JSON curJson : curJsonList) {
					Tools.assert0(curJson.isArray(), "expect an JSONArray : " + curJson.toString() );
					JSONArray prevArr = (JSONArray) curJson;
					IdxIterator idxIterator = getIdxIteratorByOperand(prev, prevArr);
					// collect data
					while(idxIterator.hasNext() ) {
						Object resEle = prevArr.opt(idxIterator.next());
						if(resEle != null) {
							res.add(resEle);
						}
					}
				}
			} else {
				for(JSON curJson : curJsonList) {
					Tools.assert0(! curJson.isArray(), "expect an JSONObject : " + curJson.toString() );
					Object resEle = curJson;
					if(resEle != null) {
						res.add(resEle);
					}
				}
			}
		}
	}
	
	// --------------- bean --------------------
	// ����pattern��ȡ����Operand
	static class Operand {
		String key;
		String left;
		String right;
		Operand next;
		
		// ��ʼ��
		public Operand() {
			
		}
		public Operand(String key, String left, String right, Operand next) {
			this.key = key;
			this.left = left;
			this.right = right;
			this.next = next;
		}
		
		// ��ǰOperand�������Ƿ�������
		public boolean isArray() {
			return left != null;
		}
		// for debug ..
		public String toString() {
			return new JSONObject()
					.element("key", key).element("left", left).element("right", right)
					.toString() ;
		}
	}
	// ����ʱ�õ���Operand [not use]
	static class RuntimeOperand {
		String key;
		IdxIterator idxIterator;
		Operand next;
		
		// ��ʼ��
		public RuntimeOperand() {
			
		}
		public RuntimeOperand(String key, IdxIterator idxIterator, Operand next) {
			this.key = key;
			this.idxIterator = idxIterator;
			this.next = next;
		}
		
		// ��ǰOperand�������Ƿ�������
		public boolean isArray() {
			return idxIterator != null;
		}
		// for debug ..
		public String toString() {
			return new JSONObject()
					.element("key", key).element("isArray", String.valueOf(idxIterator != null) )
					.toString() ;
		}
	}
	
}
