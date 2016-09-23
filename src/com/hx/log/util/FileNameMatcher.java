/**
 * file name : FileNameMatcher.java
 * created at : 5:02:38 PM Nov 29, 2015
 * created by 970655147
 */

package com.hx.log.util;

// FileNameMatcher
public final class FileNameMatcher {

	// disable constructor
	private FileNameMatcher() {
		Tools.assert0("can't instantiate !");
	}
	
	// ��֧�ֵ�ͨ���, '?' ���Ա�ʾһ�������ַ�, '*' ��ʾ����������ַ�[�����ƥ����isGreedy����Լ��]
	// ��Ϊ��ͨ��������, û��'?', '*' ���ļ���, ��������û��д'?', '*'����ı�ʾ[��� Ҫд�Ļ�, ģ��ת���][������wildCard��ʱ��, ��������ת���'?', '*', ��ƥ���ʱ��, ��'\?', '\*'�滻Ϊ��ʵ���ַ�����ʾ��'?', '*' ]
	// һ��pattern�и�����pattern�ķָ���, ������pattern֮��Ĺ�ϵΪ "��·��"
	// ���� ��û��Լ��"��·��"�ĳ���[���ȼ����� �ᵼ���ܶ�����], ������ʹ��match����ʵ�ְ�
	public static final Character MATCH_ONE = '?';
	public static final Character MATCH_MULTI = '*';
	public static final char[] WILDCARDS = new char[]{MATCH_ONE, MATCH_MULTI };
	public static final int MATCH_ONE_IDX = 0;
	public static final int MATCH_MULTI_IDX = 1;
	
	public static final String PATTERN_SEP = "\\|";
	
	/**
	 * @Name: match 
	 * @Description: �жϸ�����fileName�Ƿ�ƥ�������pattern
	 * @param fileName
	 * @param pattern
	 * @param isGreedy ��ʾ�Ƿ����̰��ƥ���ģʽ[Ҳ����ͨ���'*'�Ƿ�̰��]
	 * @return  
	 * @Create at 2016��8��6�� ����3:15:28 by '970655147'
	 */
	public static boolean match(String fileName, String pattern, boolean isGreedy) {
		// add params' verify at 2016.08.28		
        if (fileName == null && pattern == null) {
            return true;
        }
        if (fileName == null || pattern == null) {
            return false;
        }
        
		String[] subPatterns = pattern.split(PATTERN_SEP);
		for(int i=0; i<subPatterns.length; i++) {
			if(match0(fileName, subPatterns[i], isGreedy) ) {
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * @Name: match 
	 * @Description: isGreedy Ĭ��Ϊfalse
	 * @param fileName
	 * @param pattern
	 * @return  
	 * @Create at 2016��8��6�� ����3:16:51 by '970655147'
	 */
	public static boolean match(String fileName, String pattern) {
		return match(fileName, pattern, false);
	}
	
	/**
	 * @Name: equalsIgnoreCase 
	 * @Description: �ж�str01, str02�Ƿ����
	 * @param str01
	 * @param str02
	 * @return  
	 * @Create at 2016��8��6�� ����3:20:51 by '970655147'
	 */
	public static boolean equalsInRange(String str01, int start01, String str02, int start02, int len) {
//		return str01.equalsIgnoreCase(str02);
		for(int i=0; i<len; i++) {
			if(str01.charAt(start01+i) != str02.charAt(start02+i) ) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * @Name: isEmpty 
	 * @Description: �ж��������ַ����Ƿ�Ϊ���ַ���
	 * @param str
	 * @return  
	 * @Create at 2016��8��6�� ����3:21:01 by '970655147'
	 */
	public static boolean isEmpty(String str) {
		return (str == null) || "".equals(str.trim());
	}
	
	/**
	 * @Name: match0 
	 * @Description: match�ĺ���ҵ�񷽷�
	 * @param fileName
	 * @param pattern
	 * @param isGreedy
	 * @return  
	 * @Create at 2016��8��6�� ����3:28:45 by '970655147'
	 */
	private static boolean match0(String fileName, String pattern, boolean isGreedy) {
		pattern = preparePattern(pattern);
		
		int[] nextWildCards = newWildCards();
		initWildCards(pattern, nextWildCards);
		WildCardAndIdx wildCardAndIdx = new WildCardAndIdx();
		
		int fileNameIdx = 0, patternIdx = 0;
		while(hasNextWildCards(nextWildCards) ) {
			// ���û��ƥ���pattern���ַ���, ֱ�ӷ���false
			if((fileNameIdx < 0) || (fileNameIdx >= fileName.length()) ) {
				return false;
			}
			
			nextWildCard(pattern, nextWildCards, wildCardAndIdx);
			int len = wildCardAndIdx.pos - patternIdx;
			if(len != 0) {
				// ���fileName������, ���ߵ���һ��ͨ���֮����ַ�����pattern��ƥ��, ֱ�ӷ���false
				if(fileNameIdx+len >= fileName.length()) {
					return false;
				}
				if(! equalsInRange(fileName, fileNameIdx, pattern, patternIdx, len) ) {
					return false;
				}
			}
			
			// �������ͨ����ĳ���
			switch(wildCardAndIdx.wildCardIdx) {
				// ����'?', �������� : ��ȷƥ����ַ����ĳ���+1
				case MATCH_ONE_IDX :
				{
					fileNameIdx += (len + 1);
					patternIdx += (len + 1);
					break ;
				}
				// ����'*', ��Ϊ̰�� �ͷ�̰�����д���
				case MATCH_MULTI_IDX :
				{
					int curPos = wildCardAndIdx.pos;
					peekNextWildCard(fileName, nextWildCards, wildCardAndIdx);
					String strBetweenNextWildCard = null;
					// �����һ���ַ�ҲΪͨ���, ��strBetweenNextWildCardΪ"", isEmpty(strBetweenNextWildCard), ֱ�ӷ�����true, �Լ������fileNameIdx�ĸ������Ӱ��  ���ɴ���
					// ���� ��ҪԤ����pattern, ��ֹ���Ƶ�������� "**", "*?"
					if(wildCardAndIdx.pos != -1) {
						strBetweenNextWildCard = pattern.substring(curPos+1, wildCardAndIdx.pos);
					} else {
						strBetweenNextWildCard = pattern.substring(curPos+1);
					}
					// ����pattern�����һ���ַ�Ϊ*�ĳ���
					if(isEmpty(strBetweenNextWildCard) ) {
						return true;
					}
					
					if(isGreedy) {
						int prevFileNameIdx = fileNameIdx;
						fileNameIdx = fileName.lastIndexOf(strBetweenNextWildCard);
						// have no match with 'strBetweenNextWildCard' after fileNameIdx, cut off for next loop
						// updated at 2016.08.29
						if(fileNameIdx <= prevFileNameIdx) {
							fileNameIdx = -1;
						}
					} else {
						// if have no match with 'strBetweenNextWildCard' after fileNameIdx, 'fileNameIdx' will be '-1'
						fileNameIdx = fileName.indexOf(strBetweenNextWildCard, fileNameIdx+1);
					}
					patternIdx += (len + 1);
					break ;
				}
				// Other ??  can't be there in normal case
				default :
					throw new RuntimeException("unsupported wildcard !");
			}
		}
		
		return equalsInRange(fileName, fileNameIdx, pattern, patternIdx, Math.min(fileName.length()-fileNameIdx, pattern.length()-patternIdx) );
	}

	// Ԥ����pattern
	// 1. ��ֹ���Ƶ�������� "**", "*?"
	private static String preparePattern(String pattern) {
		StringBuilder sb = new StringBuilder();
		for(int i=0, len=pattern.length(); i<len; i++) {
			char ch = pattern.charAt(i);
			sb.append(ch);
			// '*XX'
			if(ch == MATCH_MULTI) {
				int nextI = i+1;
				while((nextI < len) && (contains(WILDCARDS, pattern.charAt(nextI))) ) {
					nextI ++;
				}
				i = nextI - 1;
			}
		}
		
		return sb.toString();
	}
	// ����һ��ͨ���������������
	private static int[] newWildCards() {
		return new int[WILDCARDS.length];
	}
	// ��ʼ��ͨ�����λ��
	private static void initWildCards(String pattern, int[] nextWildCards) {
		for(int i=0; i<nextWildCards.length; i++) {
			nextWildCards[i] = pattern.indexOf(WILDCARDS[i] );
		}
	}
	// �ж��Ƿ�����һ��ͨ���
	private static boolean hasNextWildCards(int[] nextWildCards) {
		for(int i=0; i<nextWildCards.length; i++) {
			if(nextWildCards[i] >= 0) {
				return true;
			}
		}
		
		return false;
	}
	// ��ȡpattern����һ��ͨ�����λ��, �����¸�ͨ�������һ��λ��
	private static void nextWildCard(String pattern, int[] nextWildCards, WildCardAndIdx wildCardAndIdx) {
		peekNextWildCard(pattern, nextWildCards, wildCardAndIdx);
		
		nextWildCards[wildCardAndIdx.wildCardIdx] = pattern.indexOf(WILDCARDS[wildCardAndIdx.wildCardIdx], wildCardAndIdx.pos+1);
	}
	// ��ȡpattern����һ��ͨ���������, �ŵ�wildCardAndIdx��
	private static void peekNextWildCard(String fileName, int[] nextWildCards, WildCardAndIdx wildCardAndIdx) {
		int next = getMinIdx(nextWildCards);
		wildCardAndIdx.wildCardIdx = next;
		
		if(next != -1) {
			wildCardAndIdx.pos = nextWildCards[next];
		} else {
			wildCardAndIdx.pos = -1;
		}
	}
	// ��ȡpattern����һ����ͨ���������
	private static int getMinIdx(int[] nextWildCards) {
		int min = Integer.MAX_VALUE, idx = -1;
		for(int i=0; i<nextWildCards.length; i++) {
			// '>= 0' for check valid 
			if((nextWildCards[i] >= 0) && (nextWildCards[i] < min) ) {
				idx = i;
				min = nextWildCards[i];
			}
		}
		
		return idx;
	}
	// �жϸ������ַ��������Ƿ�����������ַ�
	private static boolean contains(char[] chars, char ch) {
		for(int i=0; i<chars.length; i++) {
			if(chars[i] == ch) {
				return true;
			}
		}

		return false;
	}
	
	// ͨ���������, �Լ��䵱ǰλ��
	static class WildCardAndIdx {
		public int wildCardIdx;
		public int pos;
		
		public String toString() {
			return WILDCARDS[wildCardIdx] + " -> " + pos;
		}
	}
	
}
