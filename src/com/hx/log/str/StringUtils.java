/**
 * file name : StringUtils.java
 * created at : 22:18:22 2016-12-30
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class StringUtils {

    // disable constructor
    private StringUtils() {
        Tools.assert0("can't instantiate !");
    }


    // 如果给定的字符串以startsWith, 则移除startsWith
    public static String removeIfStartsWith(String str, String startsWith) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(startsWith != null, "'startsWith' can't be null ");

        if (str.startsWith(startsWith)) {
            return str.substring(startsWith.length());
        }

        return str;
    }

    public static String removeIfEndsWith(String str, String endsWith) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(endsWith != null, "'endsWith' can't be null ");

        if (str.endsWith(endsWith)) {
            return str.substring(0, str.length() - endsWith.length());
        }

        return str;
    }

    public static String addIfNotStartsWith(String str, String startsWith) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(startsWith != null, "'startsWith' can't be null ");

        if (!str.startsWith(startsWith)) {
            return startsWith + str;
        }

        return str;
    }

    public static String addIfNotEndsWith(String str, String endsWith) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(endsWith != null, "'endsWith' can't be null ");

        if (!str.endsWith(endsWith)) {
            return str + endsWith;
        }

        return str;
    }

    // 移除掉sb的添加的最后一个分隔符
    public static void removeLastSep(StringBuilder sb, String lastSep) {
        if (sb.length() > lastSep.length()) {
            sb.delete(sb.length() - lastSep.length(), sb.length());
        }
    }

    public static boolean isEmpty(String str) {
        return (str == null) || Tools.EMPTY_STR_CONDITIONS.contains(str.trim());
    }


    // 获取str中以start 和end之间的字符串
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

    public static String getStrInRange(String str, String start, String end, boolean includeStart, boolean includeEnd) {
        Tools.assert0(str != null, "'str' can't be null ");
        Tools.assert0(start != null, "'start' can't be null ");
        Tools.assert0(end != null, "'end' can't be null ");

        int startIdx = str.indexOf(start);
        if (startIdx == -1) {
            return Tools.EMPTY_STR;
        }

        int endIdx = str.indexOf(end, startIdx + start.length());
        if (endIdx == -1) {
            return Tools.EMPTY_STR;
        }

        if (!includeStart) {
            startIdx += start.length();
        }
        if (includeEnd) {
            endIdx += end.length();
        }

        return str.substring(startIdx, endIdx);
    }

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

        return Tools.EMPTY_STR;
    }

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

        return Tools.EMPTY_STR;
    }
    // 整合这三类方法, 之前的实现有点冗余			--2015.12.17
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


    // 空格类字符
    static Set<Character> SPACES = new HashSet<>();

    static {
        SPACES.add(Tools.SPACE);
        SPACES.add(Tools.TAB);
        SPACES.add(Tools.CR);
        SPACES.add(Tools.LF);
    }

    // 将字符串的多个连续的空格转换为一个空格
    // 思路 : 如果str为null  直接返回null
    // 将str中多个相邻的空格替换为一个空格[SPACE]
    // 如果结果的字符串长度为1 并且该字符为空格, 则直接返回空字符串
    // 否则  去掉前后的空格, 返回之间的子字符串
    // 可以直接使用正则进行处理		// str.replaceAll("\\s+", " ");
    public static String trimSpacesAsOne(String str) {
        if (isEmpty(str)) {
            return Tools.EMPTY_STR;
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
            return Tools.EMPTY_STR;
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

    public static String[] trimSpacesAsOne(String[] arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.length; i++) {
            arr[i] = trimSpacesAsOne(arr[i]);
        }

        return arr;
    }

    public static List<String> trimSpacesAsOne(List<String> arr) {
        Tools.assert0(arr != null, "'arr' can't be null ");
        for (int i = 0; i < arr.size(); i++) {
            arr.set(i, trimSpacesAsOne(arr.get(i)));
        }

        return arr;
    }

    public static String trimAllSpaces(String str, Map<Character, Character> escapeMap) {
        if (isEmpty(str)) {
            return Tools.EMPTY_STR;
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
    public static boolean STD_CASE_TO_UPPERCASE = false;

    // 获取标准的大写 或者小写
    public static String getStdCase(String str) {
        return getStdCase(str, STD_CASE_TO_UPPERCASE);
    }

    public static String getStdCase(String str, boolean isUpperCase) {
        Tools.assert0(str != null, "'str' can't be null ");
        if (isUpperCase) {
            return str.toUpperCase();
        } else {
            return str.toLowerCase();
        }
    }

    // 判断str01 和str02是否相同[忽略大小写]
    public static boolean equalsIgnoreCase(String str01, String str02) {
//		return getStdCase(str01).equals(getStdCase(str02) );
        // updated at 2016.06.28
        if (str01 == null && str02 == null) {
            return true;
        } else if (str01 != null) {
            return str01.equalsIgnoreCase(str02);
        } else if (str02 != null) {
            return str02.equalsIgnoreCase(str01);
        }

        // can't got there
        return false;
    }

    // 如果给定的字符串的首字母是大写的话, 将其转换为小写
    public static String lowerCaseFirstChar(String str) {
        Tools.assert0(((str != null) || (str.length() == 0)), "'str' is null ");
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
     * @param str 给定的字符串
     * @param src 需要替换的原字符串
     * @param dst 替换到的目标字符串
     * @return
     * @Name: replaceO
     * @Description: 替换给定的字符串为目标字符串
     * 为了增加HXAttrHandler.replaceO[replaceOriginal]而添加
     * @Create at 2016-09-30 21:51:15 by '970655147'
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
     * @param str    给定的字符串
     * @param mapper 需要映射的字符串kv对
     * @return
     * @Name: replaceO
     * @Description: 同时替换多个字符串
     * [这里 可能会出现WordSeprator的一些问题, 因此 可以借此机会修正修正]
     * @Create at 2016-09-30 22:21:53 by '970655147'
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
     * @param line 给定的行
     * @return
     * @Name: isCommentLine
     * @Description: 判断给定的line是否是单行注释[//, --, #, ;]
     * @Create at 2016-11-23 21:51:39 by '970655147'
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


}
