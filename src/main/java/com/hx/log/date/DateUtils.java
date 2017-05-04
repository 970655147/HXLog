/**
 * file name : DateUtils.java
 * created at : 23:08:54 2016-12-30
 * created by 970655147
 */

package com.hx.log.date;

import com.hx.log.util.Constants;
import com.hx.log.util.Tools;

/**
 * ������صĹ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:37 PM
 */
public final class DateUtils {

    // disable constructor
    private DateUtils() {
        Tools.assert0("can't instantiate !");
    }


    /**
     * ��ȡUNIXʱ���
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static long now() {
        return System.currentTimeMillis();
    }

    /**
     * ��ȡUNIXʱ�����Ӧ���ַ���
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String nowStr() {
        return String.valueOf(now());
    }

    /**
     * ��ʽ�����ڵ�ʱ��
     *
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String formatedNowStr() {
        return Constants.DATE_FORMAT.format(now());
    }

    /**
     * �������� ����start��ʱ�� ������ʱ��[ms]
     *
     * @param start ��ʼ��ʱ���
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static long spent(long start) {
        return now() - start;
    }

    /**
     * �������� ����start��ʱ�� ������ʱ����ַ�����ʾ[ms]
     *
     * @param start ��ʼ��ʱ���
     * @return long
     * @author Jerry.X.He
     * @date 5/4/2017 11:38 PM
     * @since 1.0
     */
    public static String spentStr(long start) {
        return String.valueOf(spent(start));
    }

}
