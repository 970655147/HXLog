/**
 * file name : IdxUtils.java
 * created at : 23:06:17 2016-12-30
 * created by 970655147
 */

package com.hx.log.idx;

import com.hx.log.collection.MapUtils;
import com.hx.log.util.Tools;

import java.util.Collection;

import static com.hx.log.util.Tools.assert0;

/**
 * ������صĹ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:53 PM
 */
public final class IdxUtils {

    // disable constructor
    private IdxUtils() {
        assert0("can't instantiate !");
    }

    // ----------------- ��ȡ������ط��� -----------------------

    // add at 2016.05.07

    /**
     * ���ݸ�����Ŀ������, ������Ч������[Ĭ��ȡMapUtils.GET_INFO_FROM_JSON_DEFAULT_IDX]
     *
     * @param idx   ����������
     * @param idxes ������Ŀ�������б�
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 11:52 PM
     * @since 1.0
     */
    public static int getIdx(int idx, String[] idxes) {
        return getIdx(idx, idxes.length);
    }

    public static int getIdx(int idx, Collection<String> idxes) {
        return getIdx(idx, idxes.size());
    }

    public static int getIdx(int idx, int maxSize) {
        return getIdx(idx, maxSize, MapUtils.GET_INFO_FROM_JSON_DEFAULT_IDX);
    }

    public static int getIdx(int idx, String[] idxes, int defaultIdx) {
        return getIdx(idx, idxes.length, defaultIdx);
    }

    public static int getIdx(int idx, Collection<String> idxes, int defaultIdx) {
        return getIdx(idx, idxes.size(), defaultIdx);
    }

    public static int getIdx(int idx, int maxSize, int defaultIdx) {
        return (idx >= maxSize) ? defaultIdx : idx;
    }

    // ----------------- ���� -----------------------

}
