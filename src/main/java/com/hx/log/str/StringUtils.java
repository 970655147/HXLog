/**
 * file name : StringUtils.java
 * created at : 22:18:22 2016-12-30
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.common.str.WordsSeprator;
import com.hx.common.util.InnerTools;
import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.hx.log.util.Tools.EMPTY_STR;

/**
 * �ַ����������ع���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 4:36 PM
 */
public final class StringUtils {

    // disable constructor
    private StringUtils() {
        Tools.assert0("can't instantiate !");
    }

    /**
     * ����������ַ�����startsWith��ͷ, ���Ƴ�startsWith
     *
     * @param str        �������ַ���
     * @param startsWith ������ǰ׺
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:36 PM
     * @since 1.0
     */
    public static String removeIfStartsWith(String str, String startsWith) {
        return InnerTools.removeIfStartsWith(str, startsWith);
    }

    /**
     * ����������ַ�����endsWith��ͷ, ���Ƴ�endsWith
     *
     * @param str      �������ַ���
     * @param endsWith �����ĺ�׺
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:54 PM
     * @since 1.0
     */
    public static String removeIfEndsWith(String str, String endsWith) {
        return InnerTools.removeIfEndsWith(str, endsWith);
    }

    /**
     * ����������ַ�������startsWith��ͷ, �����startsWith
     *
     * @param str        �������ַ���
     * @param startsWith ������ǰ׺
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:55 PM
     * @since 1.0
     */
    public static String addIfNotStartsWith(String str, String startsWith) {
        return InnerTools.addIfNotStartsWith(str, startsWith);
    }

    /**
     * ����������ַ�������endsWith��ͷ, �����endsWith
     *
     * @param str      �������ַ���
     * @param endsWith �����ĺ�׺
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:54 PM
     * @since 1.0
     */
    public static String addIfNotEndsWith(String str, String endsWith) {
        return InnerTools.addIfNotEndsWith(str, endsWith);
    }

    /**
     * �Ƴ���sb����ӵ����һ���ָ���
     *
     * @param sb      �������ַ���
     * @param lastSep ���һ���ָ���
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 4:55 PM
     * @since 1.0
     */
    public static void removeLastSep(StringBuilder sb, String lastSep) {
        InnerTools.removeLastSep(sb, lastSep);
    }

    /**
     * �жϸ������ַ����Ƿ�Ϊ��
     *
     * @param str �������ַ���
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 4:56 PM
     * @since 1.0
     */
    public static boolean isEmpty(String str) {
        return (str == null) || Tools.EMPTY_STR_CONDITIONS.contains(str.trim());
    }

    /**
     * ��ȡstr����start ��end֮����ַ���
     *
     * @param str          �������ַ���
     * @param start        ������start
     * @param end          ������end
     * @param includeStart ����Ƿ���Ҫ����start
     * @param includeEnd   ����Ƿ���Ҫ����end
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRange(String str, String start, String end, boolean includeStart, boolean includeEnd) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(start != null, "'start' can't be null ");
        Tools.assert0(end != null, "'end' can't be null ");

        int startIdx = str.indexOf(start);
        if (startIdx == -1) {
            return EMPTY_STR;
        }

        int endIdx = str.indexOf(end, startIdx + start.length());
        if (endIdx == -1) {
            return EMPTY_STR;
        }

        if (!includeStart) {
            startIdx += start.length();
        }
        if (includeEnd) {
            endIdx += end.length();
        }

        return str.substring(startIdx, endIdx);
    }

    /**
     * ��ȡstr��str�Ӵ�֮��Ĳ���
     *
     * @param str     �������ַ���
     * @param start   �������Ӵ�
     * @param include �Ƿ���Ҫ����start
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRangeWithStart(String str, String start, boolean include) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(start != null, "'start' can't be null ");

        int idx = str.indexOf(start);
        if (idx != -1) {
            if (!include) {
                idx += start.length();
            }
            return str.substring(idx);
        }

        return EMPTY_STR;
    }

    /**
     * ��ȡstr��end�Ӵ�֮��Ĳ���
     *
     * @param str     �������ַ���
     * @param end     �������Ӵ�
     * @param include �Ƿ���Ҫ����start
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:57 PM
     * @since 1.0
     */
    public static String getStrInRangeWithEnd(String str, String end, boolean include) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(end != null, "'end' can't be null ");

        int idx = str.indexOf(end);
        if (idx != -1) {
            if (include) {
                idx += end.length();
            }
            return str.substring(0, idx);
        }

        return EMPTY_STR;
    }

    public static String getStrInRange(String str, String start, String end) {
        return getStrInRange(str, start, end, false, false);
    }

    public static String getStrInRangeInclude(String str, String start, String end) {
        return getStrInRange(str, start, end, true, true);
    }

    public static String getStrInRangeWithStart(String str, String start) {
        return getStrInRangeWithStart(str, start, false);
    }

    public static String getStrInRangeWithStartInclude(String str, String start) {
        return getStrInRangeWithStart(str, start, true);
    }

    public static String getStrInRangeWithEnd(String str, String end) {
        return getStrInRangeWithEnd(str, end, false);
    }

    public static String getStrInRangeWithEndInclude(String str, String end) {
        return getStrInRangeWithEnd(str, end, true);
    }

    // ���������෽��, ֮ǰ��ʵ���е�����			--2015.12.17
//	public static String getStrInRange(String str, String start, String end) {
////		int startIdx = str.indexOf(start);
////		if(startIdx == -1) {
////			return Tools.EMPTY_STR;
////		}
////		
////		int endIdx = str.indexOf(end, startIdx + start.length());
////		if(endIdx == -1) {
////			return Tools.EMPTY_STR;
////		}
////		
////		return str.substring(startIdx + start.length(), endIdx);
//		return getStrInRange0(str, start, end, false, false);
//	}


    /**
     * �ո����ַ�
     */
    static Set<Character> SPACES = Tools.asSet(Tools.SPACE, Tools.TAB, Tools.CR, Tools.LF);

    /**
     * ���ַ����Ķ�������Ŀո�ת��Ϊһ���ո�
     * ˼· : ���strΪnull  ֱ�ӷ���null
     * ��str�ж�����ڵĿո��滻Ϊһ���ո�[SPACE]
     * ���������ַ�������Ϊ1 ���Ҹ��ַ�Ϊ�ո�, ��ֱ�ӷ��ؿ��ַ���
     * ����  ȥ��ǰ��Ŀո�, ����֮������ַ���
     * ����ֱ��ʹ��������д���		// str.replaceAll("\\s+", " ");
     *
     * @param str �������ַ���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 4:59 PM
     * @since 1.0
     */
    public static String trimSpacesAsOne(String str) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            if (SPACES.contains(str.charAt(i))) {
                sb.append(Tools.SPACE);
                int nextI = i + 1;
                while ((nextI < str.length()) && SPACES.contains(str.charAt(nextI))) nextI++;
                i = nextI - 1;
                continue;
            }
            sb.append(str.charAt(i));
        }

        if ((sb.length() == 0) || ((sb.length() == 1) && SPACES.contains(sb.charAt(0)))) {
            return EMPTY_STR;
        } else {
            int start = 0, end = sb.length();
            if (SPACES.contains(sb.charAt(start))) {
                start++;
            }
            if (SPACES.contains(sb.charAt(end - 1))) {
                end--;
            }

            return sb.substring(start, end);
        }
    }

    /**
     * ��arr�е��ַ��� �����ڵĶ���ո�ϲ�Ϊһ��
     *
     * @param arr �������ַ�������
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String[] trimSpacesAsOne(String[] arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.length; i++) {
            arr[i] = trimSpacesAsOne(arr[i]);
        }

        return arr;
    }

    /**
     * ��arr�е��ַ��� �����ڵĶ���ո�ϲ�Ϊһ��
     *
     * @param arr �������ַ�������
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static List<String> trimSpacesAsOne(List<String> arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.size(); i++) {
            arr.set(i, trimSpacesAsOne(arr.get(i)));
        }

        return arr;
    }

    /**
     * ȥ��str�е����пո�, ����escape��ص��ַ�
     *
     * @param str       �������ַ�
     * @param escapeMap ��Ҫ�������ַ�pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String trimAllSpaces(String str, Map<Character, Character> escapeMap) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            Character ch = str.charAt(i);
            if ((escapeMap != null) && escapeMap.containsKey(ch)) {
                int prevI = i;
                i = str.indexOf(escapeMap.get(ch), i + 1);
                if (i >= 0) {
                    sb.append(str.substring(prevI, i + 1));
                } else {
                    sb.append(str.substring(prevI));
                    break;
                }
                continue;
            }
            if (SPACES.contains(str.charAt(i))) {
                int nextI = i + 1;
                while ((nextI < str.length()) && SPACES.contains(str.charAt(nextI))) nextI++;
                i = nextI - 1;
                continue;
            }
            sb.append(str.charAt(i));
        }
        return sb.toString();
    }

    public static String trimAllSpaces(String str) {
        return trimAllSpaces(str, null);
    }

    /**
     * ȥ��arr�е����е��ַ��������пո�
     *
     * @param arr       �������ַ�������
     * @param escapeMap ��Ҫ�������ַ�pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String[] trimAllSpaces(String[] arr, Map<Character, Character> escapeMap) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.length; i++) {
            arr[i] = trimAllSpaces(arr[i], escapeMap);
        }

        return arr;
    }

    public static String[] trimAllSpaces(String[] arr) {
        return trimAllSpaces(arr, null);
    }

    /**
     * ȥ��arr�е����е��ַ��������пո�
     *
     * @param arr       �������ַ�������
     * @param escapeMap ��Ҫ�������ַ�pair
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static List<String> trimAllSpaces(List<String> arr, Map<Character, Character> escapeMap) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.size(); i++) {
            arr.set(i, trimAllSpaces(arr.get(i), escapeMap));
        }

        return arr;
    }

    public static List<String> trimAllSpaces(List<String> arr) {
        return trimAllSpaces(arr, null);
    }


    /**
     * ȥ��str�еĸ������ַ�
     *
     * @param str            �������ַ���
     * @param needBeFiltered ��Ҫ�����˵����ַ�
     * @return java.lang.String[]
     * @author Jerry.X.He
     * @date 5/5/2017 5:01 PM
     * @since 1.0
     */
    public static String filter(String str, Set<Character> needBeFiltered) {
        if (isEmpty(str) || Tools.isEmpty(needBeFiltered)) {
            return null;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (!needBeFiltered.contains(str.charAt(i))) {
                sb.append(str.charAt(i));
            }
        }

        return trimSpacesAsOne(sb.toString());
    }


    /**
     * ��sb�����str���ַ���, �����Ҫclean, ����clean sb�е��ַ�, �����Ҫ���crlf, ��ӻس�
     *
     * @param sb         ������stringBuilder
     * @param str        ��Ҫ��ӵ��ַ���
     * @param isClean    ����ַ���֮ǰ�Ƿ���Ҫclean sb
     * @param appendCRLF ����ַ���֮�� �Ƿ���Ҫ���crlf
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 5:04 PM
     * @since 1.0
     */
    public static void append(StringBuilder sb, String str, boolean isClean, boolean appendCRLF) {
        Tools.assert0(sb != null, "'sb' can't be null ");
        if (isClean) {
            sb.setLength(0);
        }
        sb.append(str);
        if (appendCRLF) {
            sb.append(Tools.CRLF);
        }
    }

    public static void append(StringBuilder sb, String str, boolean isClean) {
        append(sb, str, isClean, false);
    }

    public static void append(StringBuilder sb, String str) {
        append(sb, str, false);
    }

    public static void appendCRLF(StringBuilder sb, String str, boolean isClean) {
        append(sb, str, isClean, true);
    }

    public static void appendCRLF(StringBuilder sb, String str) {
        appendCRLF(sb, str, false);
    }


    // add at 2016.05.18
    /**
     * ��׼��case
     */
    public static boolean STD_CASE_TO_UPPERCASE = false;

    /**
     * ��ȡ��׼�Ĵ�д ����Сд
     *
     * @param str         �������ַ���
     * @param isUpperCase �Ƿ�ת��Ϊ��д
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:07 PM
     * @since 1.0
     */
    public static String getStdCase(String str, boolean isUpperCase) {
        Tools.assert0(str != null, "'str' can't be null ");
        if (isUpperCase) {
            return str.toUpperCase();
        } else {
            return str.toLowerCase();
        }
    }

    public static String getStdCase(String str) {
        return getStdCase(str, STD_CASE_TO_UPPERCASE);
    }

    //

    /**
     * �ж�str01 ��str02�Ƿ���ͬ[���Դ�Сд]
     *
     * @param str01 �ַ���01
     * @param str02 �ַ���02
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 5:08 PM
     * @since 1.0
     */
    public static boolean equalsIgnoreCase(String str01, String str02) {
        return InnerTools.equalsIgnoreCase(str01, str02);
    }

    /**
     * ����������ַ���������ĸ�Ǵ�д�Ļ�, ����ת��ΪСд
     *
     * @param str �������ַ���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:09 PM
     * @since 1.0
     */
    public static String lowerCaseFirstChar(String str) {
        Tools.assert0(!isEmpty(str), "'str' is null ");
        if (str.length() == 1) {
            return str.toLowerCase();
        }
        if (Character.isUpperCase(str.charAt(0))) {
            return Character.toLowerCase(str.charAt(0)) + str.substring(1);
        }

        return str;
    }

    public static String upperCaseFirstChar(String str) {
        Tools.assert0(((str != null) || (str.length() == 0)), "'str' is null ");
        if (str.length() == 1) {
            return str.toUpperCase();
        }
        if (Character.isLowerCase(str.charAt(0))) {
            return Character.toUpperCase(str.charAt(0)) + str.substring(1);
        }

        return str;
    }


    // add at 2016.08.11

    /**
     * �滻�������ַ���ΪĿ���ַ���
     * Ϊ������HXAttrHandler.replaceO[replaceOriginal]�����
     *
     * @param str �������ַ���
     * @param src ��Ҫ�滻��ԭ�ַ���
     * @param dst �滻����Ŀ���ַ���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:11 PM
     * @since 1.0
     */
    public static String replaceO(String str, String src, String dst) {
        Tools.assert0(str != null, "'str' can't be null !");
        Tools.assert0(src != null, "'src' can't be null !");
        Tools.assert0(dst != null, "'dst' can't be null !");

        StringBuilder sb = new StringBuilder(str.length());
        int idx = 0;
        while (idx >= 0) {
            int nextSrc = str.indexOf(src, idx);
            if (nextSrc >= 0) {
                sb.append(str.substring(idx, nextSrc));
                sb.append(dst);
                idx = nextSrc + src.length();
            } else {
                sb.append(str.substring(idx));
                idx = -1;
            }
        }

        return sb.toString();
    }

    /**
     * ͬʱ�滻����ַ���
     * [���� ���ܻ����WordSeprator��һЩ����, ��� ���Խ�˻�����������]
     *
     * @param str    �������ַ���
     * @param mapper ��Ҫӳ����ַ���kv��
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/5/2017 5:12 PM
     * @since 1.0
     */
    public static String replaceO(String str, Map<String, String> mapper) {
        Tools.assert0(str != null, "'str' can't be null !");
        Tools.assert0(mapper != null, "'mapper' can't be null !");

        StringBuilder sb = new StringBuilder(str.length());
        WordsSeprator sep = new WordsSeprator(str, mapper.keySet(), null, true, true);

        boolean isSep = false;
        while (sep.hasNext()) {
            if (!isSep) {
                sb.append(sep.next());
            } else {
                String sepNow = sep.next();
                sb.append(mapper.get(sepNow));
            }
            isSep = !isSep;
        }

        return sb.toString();
    }

    // add at 2016.11.23

    /**
     * �жϸ�����line�Ƿ��ǵ���ע��[//, --, #, ;]
     *
     * @param line ��������
     * @return boolean
     * @author Jerry.X.He
     * @date 5/5/2017 5:12 PM
     * @since 1.0
     */
    public static boolean isCommentLine(String line) {
        if (isEmpty(line)) {
            return false;
        }

        String trimmed = line.trim();
        int lim = trimmed.length() < Constants.COMMENT_MAX_LEN ? trimmed.length() : Constants.COMMENT_MAX_LEN;
        for (int i = 1; i <= lim; i++) {
            String sub = line.substring(0, i);
            if (Constants.COMMENT_MARKS.contains(sub)) {
                return true;
            }
        }
        return false;
    }


    /**
     * ת��������ַ���
     *
     * @param str            �������ַ���
     * @param needToBeFormat ��Ҫת����ַ��б�
     * @param transferChar   ת���ַ�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:45 PM
     * @since 1.0
     */
    public static String transfer(String str, Set<Character> needToBeFormat, Character transferChar) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeFormat.contains(str.charAt(i))) {
                sb.append(transferChar);
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }

    public static String transfer(String str) {
        return transfer(str, InnerTools.NEED_BE_TRANSFER, '\\');
    }

    /**
     * ת��������ַ���
     *
     * @param str            �������ַ���
     * @param needToBeFormat ��Ҫת����ַ�ӳ��
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:46 PM
     * @since 1.0
     */
    public static String transfer(String str, Map<Character, Character> needToBeFormat) {
        if (isEmpty(str)) {
            return EMPTY_STR;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeFormat.containsKey(str.charAt(i))) {
                sb.append(needToBeFormat.get(str.charAt(i)));
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }

    /**
     * ȥ����ʽ���ַ���   Ϊÿһ��'"', '\' ǰ�����һ��ת���ַ�['\']
     *
     * @param str              �������ַ���
     * @param needToBeDeformat ��Ҫȥת����ַ��б�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/7/2017 2:48 PM
     * @since 1.0
     */
    public static String detransfer(String str, Set<Character> needToBeDeformat) {
        if (isEmpty(str)) {
            return null;
        }

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            if (needToBeDeformat.contains(str.charAt(i))) {
                sb.append(str.charAt(++i));
                continue;
            }
            sb.append(str.charAt(i));
        }

        return sb.toString();
    }


}
