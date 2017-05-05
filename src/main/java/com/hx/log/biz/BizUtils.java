/**
 * file name : BizUtils.java
 * created at : 22:51:02 2016-12-30
 * created by 970655147
 */

package com.hx.log.biz;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;
import com.hx.log.biz.interf.GetLengthStrHandler;
import com.hx.log.util.Tools;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.hx.log.log.LogPatternUtils.formatLogInfoWithIdx;
import static com.hx.log.util.Log.err;
import static com.hx.log.util.Tools.assert0;

/**
 * �����ճ�ҵ��Ĺ�����
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:29 PM
 */
public final class BizUtils {

    // disable constructor
    private BizUtils() {
        assert0("can't instantiate !");
    }


    /**
     * ���е����ֵ�Character
     */
    public static final Set<Character> NUMBERS = new HashSet<>();

    static {
        for (char i = '0'; i <= '9'; i++) {
            NUMBERS.add(i);
        }
    }

    /**
     * ����۸�, Ҳ�������ڴ�����ȡ�ַ����е�BigDecimal�����
     * ��ȡ������str�е�����, ., Ȼ��ת��Ϊ BigDecimal
     *
     * @param str �������ַ���
     * @return java.math.BigDecimal
     * @author Jerry.X.He
     * @date 5/4/2017 10:30 PM
     * @since 1.0
     */
    public static BigDecimal dealPrice(String str) {
        if (Tools.isEmpty(str)) {
            return Tools.BIGDEC_ZERO;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (NUMBERS.contains(ch) || (ch == '.')) {
                sb.append(ch);
            }
        }
        if (sb.length() == 0) {
            return Tools.BIGDEC_ZERO;
        } else {
            return new BigDecimal(sb.toString());
        }
    }

    /**
     * ����ҳ��, Ҳ�������ڴ�����ȡ�ַ����е����������
     * ��ȡ������str�е�����, Ȼ��ת��Ϊ Integer
     *
     * @param str �������ַ���
     * @return java.lang.Integer
     * @author Jerry.X.He
     * @date 5/4/2017 10:30 PM
     * @since 1.0
     */
    public static Integer dealPageNum(String str) {
        if (Tools.isEmpty(str)) {
            return Tools.INTE_ZERO;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (NUMBERS.contains(ch)) {
                sb.append(ch);
            }
        }
        if (sb.length() == 0) {
            return Tools.INTE_ZERO;
        } else {
            return new Integer(sb.toString());
        }
    }


    /**
     * ƥ��siteUrl �� regex
     */
    public static final Pattern SITE_URL_PATTERN = Pattern.compile("^(\\w{3,5}://\\w+(\\.\\w+)+?/)(.*)");

    /**
     * ��ȡվ�����ҳurl
     * http://www.baidu.com/tieba/java/page01.jsp  =>  http://www.baidu.com/
     *
     * @param url ������վ�������url
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 10:31 PM
     * @since 1.0
     */
    public static String getSiteUrl(String url) {
        assert0(!Tools.isEmpty(url), "'url' can't be null ");
        Matcher matcher = SITE_URL_PATTERN.matcher(url);
        if (matcher.matches()) {
            return matcher.group(1);
        }
        return null;
    }

    /**
     * ������/, ��Ե�urlת��Ϊ���Ե�url
     * ת�� /path & ./path
     *
     * @param siteUrl      ������վ���url
     * @param relativePath ���·��
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 10:32 PM
     * @since 1.0
     */
    public static String transformUrl(String siteUrl, String relativePath) {
        assert0(!Tools.isEmpty(siteUrl), "'siteUrl' can't be null ");
        assert0(!Tools.isEmpty(relativePath), "'relativePath' can't be null ");

        if (relativePath.startsWith("/")) {
            return getSiteUrl(siteUrl) + Tools.removeIfStartsWith(relativePath, "/");
        } else if (relativePath.startsWith("./")) {
            return siteUrl.substring(0, siteUrl.lastIndexOf("/") + 1) + Tools.removeIfStartsWith(relativePath, "./");
        } else {
            return relativePath;
        }
    }

    /**
     * Ϊ nextStageParams ��� category ����ز���
     *
     * @param category        ��ǰCategory
     * @param nextStageParams ��һ��State�Ĳ���
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:33 PM
     * @since 1.0
     */
    public static void addNameUrlSite(JSONObject category, JSONObject nextStageParams) {
        assert0(!Tools.isEmpty(category), "'category' can't be null ");
        nextStageParams.put(Tools.NAME, category.getString(Tools.NAME));
        nextStageParams.put(Tools.URL, category.getString(Tools.URL));
        nextStageParams.put(Tools.SITE, nextStageParams.getString(Tools.SITE) + "." + category.getString(Tools.NAME));
    }

    /**
     * ͨ����Ʒ����Ŀ, �Լ�ÿһҳ��ʾ�Ĳ�Ʒ����Ŀ, ����ҳ��
     *
     * @param productNum ��Ʒ������
     * @param numPerPage ÿһҳ������
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 10:33 PM
     * @since 1.0
     */
    public static int calcPageNums(int productNum, int numPerPage) {
        return ((productNum - 1) / numPerPage) + 1;
    }

    /**
     * ��ȡ��ֵ�����͵����ݶ�, ��ӵ�headers��
     *
     * @param configFile Ŀ�������ļ�
     * @param headers    ���������ļ�֮��, ������ͷ��ӵ�headers
     * @param sep        Ŀ�������ļ��ķָ���
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 10:50 PM
     * @since 1.0
     */
    public static void addHeaders(File configFile, Map<String, String> headers, String sep) {
        assert0(configFile != null, "'configFile' can't be null ");
        assert0(headers != null, "'headers' can't be null ");
        assert0(sep != null, "'sep' can't be null ");

        List<String> lines = null;
        try {
            lines = Tools.getContentWithList(configFile);
        } catch (IOException e) {
            // ignore
            err(formatLogInfoWithIdx("did not find the specified configFile : {} !", configFile));
            return;
        }

        for (String line : lines) {
            if (!Tools.isCommentLine(line)) {
                int idx = line.indexOf(sep);
                if (idx > 0) {
                    headers.put(line.substring(0, idx), line.substring(idx + 1));
                }
            }
        }
    }

    /**
     * ���뺬��unicode�ַ������ַ���
     * ����һ���ַ���, Ѱ�ҳ�ƥ��"\\uxxxx"���ַ���, Ȼ�������Ϊ�ַ�[unicode -> char]
     * �����������ַ���������
     *
     * @param str
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:01 PM
     * @since 1.0
     */
    public static String unicodeDecode(String str) {
        assert0(str != null, "'str' can't be null ");

        StringBuilder sb = new StringBuilder(str.length());
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            if (ch == Tools.SLASH) {
                if (str.charAt(i + 1) == 'u') {
                    boolean isUnicode = true;
                    for (int j = 0; j < 4; j++) {
                        // '+2' escape '\\u'
                        if (!isHexChar(str.charAt(i + j + 2))) {
                            isUnicode = false;
                            break;
                        }
                    }

                    // ���"\\u"֮����ĸ��ַ����Ա�ʾΪʮ�����Ƶ�����, �������, ������i, continue
                    if (isUnicode) {
                        char decoded = Character.valueOf((char) Integer.valueOf(str.substring(i + 2, i + 6), 16).intValue());
                        sb.append(decoded);
                        i += 5;
                        continue;
                    }
                }
            }

            sb.append(ch);
        }

        return sb.toString();
    }

    /**
     * �жϸ������ַ��Ƿ�ɱ�ʾʮ������[0-9, a-f, A-F]
     *
     * @param ch �������ַ�
     * @return boolean
     * @author Jerry.X.He
     * @date 5/4/2017 11:02 PM
     * @since 1.0
     */
    public static boolean isHexChar(char ch) {
        return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
    }


    // ------------ ����ת����� --------------------

    /**
     * Ĭ�ϵĻ�ȡ�����ַ�����Handler
     */
    public static final GetLengthStrHandler DEFAULT_GET_LENGTH_STR_HANDLER = new GetLengthStrHandler() {
        public String getLengthStr(long length, String dimen) {
            return length + " " + dimen;
        }
    };

    /**
     * ���ݳ���, ��ȡ���ȵ��ַ�����ʾ
     *
     * @param length ��Ҫ����ĵ�λ����
     * @param dimen  �����ĵ�λ
     * @return the string representation
     * @author Jerry.X.He
     * @date 5/4/2017 11:05 PM
     * @since 1.0
     */
    public static String getLengthString(long length, String dimen) {
        return getLengthString(length, dimen, DEFAULT_GET_LENGTH_STR_HANDLER);
    }

    /**
     * ������Ҫ����ĵ�λ���� �͵�λ, �Լ���ȡ�����ַ�����handler, ��ȡ���
     *
     * @param length              ��Ҫ����ĵ�λ����
     * @param dimen               �����ĵ�λ
     * @param getLengthStrHandler ��ȡ�����ַ�����handler
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:06 PM
     * @since 1.0
     */
    public static String getLengthString(long length, String dimen, GetLengthStrHandler getLengthStrHandler) {
        long transfered = -1;
        if (Tools.equalsIgnoreCase(Tools.BYTE, dimen)) {
            transfered = length;
        } else if (Tools.equalsIgnoreCase(Tools.KB, dimen)) {
            transfered = Tools.getKBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.MB, dimen)) {
            transfered = Tools.getMBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.GB, dimen)) {
            transfered = Tools.getGBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.TB, dimen)) {
            transfered = Tools.getTBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.PB, dimen)) {
            transfered = Tools.getPBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.EB, dimen)) {
            transfered = Tools.getEBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.ZB, dimen)) {
            transfered = Tools.getZBytesByBytes(length);
        } else if (Tools.equalsIgnoreCase(Tools.YB, dimen)) {
            transfered = Tools.getYBytesByBytes(length);
        } else {
            assert0("unSupported Unit : " + dimen + " !");
        }

        return getLengthStrHandler.getLengthStr(transfered, dimen);
    }

    // �����ֽ���, ��ȡǧ�ֽ���, ���ֽ���, ���ֽ���, ���ֽ���
    public static long getKBytesByBytes(long bytes) {
        return bytes >> 10;
    }

    public static long getMBytesByBytes(long bytes) {
        return bytes >> 20;
    }

    public static long getGBytesByBytes(long bytes) {
        return bytes >> 30;
    }

    public static long getTBytesByBytes(long bytes) {
        return bytes >> 40;
    }

    public static long getPBytesByBytes(long bytes) {
        return bytes >> 50;
    }

    public static long getEBytesByBytes(long bytes) {
        return bytes >> 60;
    }

    public static long getZBytesByBytes(long bytes) {
        return bytes >> 70;
    }

    public static long getYBytesByBytes(long bytes) {
        return bytes >> 80;
    }


    // add at 2016.05.17
    /**
     * ��ѯ�ַ���Ĭ�ϵ�kv�ָ���
     */
    public static String PARAM_KV_SEP = "=";
    /**
     * ��ѯ�ַ����Ķ��kv��֮��ķָ���
     */
    public static String PARAM_PARAM_SEP = "&";

    /**
     * ���ӷ�װget����Ĳ�ѯ�ַ���
     *
     * @param params �����Ĳ����б�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:07 PM
     * @since 1.0
     */
    public static String encapQueryString(Map<String, String> params) {
        return encapQueryString0(params, PARAM_KV_SEP, PARAM_PARAM_SEP);
    }

    /**
     * cookie Ĭ�ϵ�kv�ָ���
     */
    public static String COOKIE_KV_SEP = "=";
    /**
     * cookie �Ķ��kv��֮��ķָ���
     */
    public static String COOKIE_COOKIE_SEP = ";";

    /**
     * ͨ��cookies��ȡcookie���ַ�����ʾ
     *
     * @param cookies ������cookie�б�
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:08 PM
     * @since 1.0
     */
    public static String getCookieStr(Map<String, String> cookies) {
        return encapQueryString0(cookies, COOKIE_KV_SEP, COOKIE_COOKIE_SEP);
    }

    /**
     * ͨ��cookie��ʽ���ַ��� ��ȡ����cookie [���� ֱ��ʹ��split, ������ִ���]
     *
     * @param cookiesStr ������cookie�ַ���
     * @return java.util.Map<java.lang.String,java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 11:09 PM
     * @since 1.0
     */
    public static Map<String, String> getCookiesByCookieStr(String cookiesStr) {
        assert0(cookiesStr != null, "'cookieStr' can't be null !");

        String[] cookies = cookiesStr.split(COOKIE_COOKIE_SEP);
        Map<String, String> res = new HashMap<>(cookies.length);
        for (int i = 0; i < cookies.length; i++) {
            String[] kvPair = cookies[i].split(COOKIE_KV_SEP);
            assert0(kvPair.length > 1, "error cookieString : '" + cookiesStr + "', around : '" + cookies[i] + "'");
            res.put(kvPair[0], kvPair[1]);
        }
        return res;
    }

    /**
     * @param e
     * @return
     * @Description: ��ȡ�������쳣�Ĵ�����Ϣ
     * @Create at 2016-12-30 23:03:08 by '970655147'
     */
    public static String errorMsg(Exception e) {
        assert0(e != null, "'e' can't be null ");
        return e.getClass().getName() + " -> " + e.getMessage();
    }


    // add at 2016.06.18
    // �շ� -> �»��߱�ʾ
    private static Character underLine = '_';

    public static String camel2UnderLine(String name) {
        assert0(name != null, "'name' can't be null ");

        StringBuilder sb = new StringBuilder(name.length() + 10);
        boolean isLastCharUpper = Character.isUpperCase(name.charAt(0));
//		boolean isLastCharUpper = false;

        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (Character.isUpperCase(ch)) {
                if (!isLastCharUpper) {
                    sb.append(underLine);
                }
                sb.append(Character.toLowerCase(ch));
                isLastCharUpper = true;
            } else {
                sb.append(ch);
                isLastCharUpper = false;
            }
        }

        return sb.toString();
    }

    /**
     * ���������»�������ת��Ϊ�շ�����
     *
     * @param name �������»�������
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:12 PM
     * @since 1.0
     */
    public static String underLine2Camel(String name) {
        assert0(name != null, "'name' can't be null ");

        StringBuilder sb = new StringBuilder(name.length() + 10);
        for (int i = 0; i < name.length(); i++) {
            char ch = name.charAt(i);
            if (! underLine.equals(ch)) {
                sb.append(ch);
                continue ;
            }

            char nextCh = name.charAt(i + 1);
            if (Character.isLowerCase(nextCh) || Character.isUpperCase(nextCh)) {
                sb.append(Character.toUpperCase(name.charAt(i + 1)));

                // skip '_', the end of loop skip 'nextCh'
                i++;
            } else {
                sb.append(ch);
            }
        }

        return sb.toString();
    }

    // add at 2016.08.25

    /**
     * ���ݸ�����class��Ȩ�޶���, ��ȡ��class�ļ�����
     *
     * @param fullName ������class��Ȩ�޶���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:11 PM
     * @since 1.0
     */
    public static String getClazzNameByFullName(String fullName) {
        String name = fullName;
        int lastIdxOfDot = fullName.lastIndexOf(".");
        if (lastIdxOfDot >= 0) {
            name = fullName.substring(lastIdxOfDot + 1);
        }

        return name;
    }

    // ----------------- �������� -----------------------

    /**
     * ���ݸ����Ĳ����б� �����ַָ���, ��װ�����б�
     *
     * @param params    �����б�
     * @param KVSep     kv֮��ķָ���
     * @param paramsSep kv��֮��÷ָ���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:10 PM
     * @since 1.0
     */
    private static String encapQueryString0(Map<String, String> params, String KVSep, String paramsSep) {
        assert0(params != null, "'params' can't be null ");
        assert0(KVSep != null, "'KVSep' can't be null ");
        assert0(paramsSep != null, "'paramsSep' can't be null ");

        StringBuilder sb = new StringBuilder();
        for (Entry<String, String> entry : params.entrySet()) {
            sb.append(entry.getKey());
            sb.append(KVSep);
            sb.append(entry.getValue());
            sb.append(paramsSep);
        }
        Tools.removeLastSep(sb, paramsSep);

        return sb.toString();
    }

}
